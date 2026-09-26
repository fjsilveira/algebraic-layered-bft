package layers

import crypto.Evidence
import domain.*
import java.security.PublicKey

/** Layer 4 for Tendermint, a pure state machine. Every read of L is a query of
  * Exp_3; no threshold, projection or evidence set appears here. Line numbers
  * refer to the pseudocode of the thesis.
  */
final class Tendermint[E <: Evidence](
    self: PublicKey,
    queries: QuorumQueries[E],
    app: Application
) extends ProtocolLogic[E]:

  private type S = ReplicaState[E]
  private type R = Registers[E]
  private type QC = TmQC[E]

  def init(): S = ReplicaState(Accumulation.empty, fresh(Map.empty, 0L))

  def onTimeout(s: S, t: Context): S =
    val r = s.registers
    if t != context(s) then s // line 3
    else
      r.currentStep match
        case Step.PROPOSAL => // line 9
          s.set(r.copy(proposal = None, currentStep = Step.PREVOTE))
        case Step.PREVOTE => // line 12
          s.set(r.copy(vote = None, currentStep = Step.PRECOMMIT))
        case Step.PRECOMMIT => // lines 15 to 19
          s.set(enterRound(r, r.currentRound + 1))

  def send(s: S): Set[Outbound[E]] =
    val r = s.registers
    val h = r.currentHeight
    def out(st: Step, x: Option[Block], a: TmMeta[E]) =
      Outbound.Payload(Slot(h, r.currentRound, st, self), Body(x, a))
    r.currentStep match
      case Step.PROPOSAL =>
        val commit = // lines 9 to 11
          if r.currentRound == 0 && h > 0 then
            r.decisions.get(h - 1).map(d => Outbound.Certificate(d.qc))
          else None
        val lead = app.getProposer(h, r.currentRound) == self
        val proposal = // lines 15 to 18
          Option.when(lead)(out(Step.PROPOSAL, r.proposal, TmMeta(r.validQC)))
        (commit ++ proposal).toSet
      case Step.PREVOTE => // lines 24 to 26
        Set(out(Step.PREVOTE, r.proposal, TmMeta.empty))
      case Step.PRECOMMIT => // lines 30 to 32
        Set(out(Step.PRECOMMIT, r.vote, TmMeta.empty))

  def fastForward(s: S, qc: QC): S =
    val r = s.registers
    val h = qc.slot.height
    if h < r.currentHeight || r.decisions.contains(h) then s // lines 5, 8
    else
      qc.value match
        case Some(b) // lines 11 to 13
            if app.isValid(b) && qc.slot.step == Step.PRECOMMIT &&
              queries.verifyQC(qc) =>
          val r1 = r.copy(decisions = r.decisions.updated(h, Decision(b, qc)))
          s.set(if h == r.currentHeight then nextHeight(r1) else r1) // 16-22
        case _ => s

  def compute(s: S): S =
    val r = s.registers
    if r.decisions.contains(r.currentHeight) then s.set(nextHeight(r)) // 20
    else
      queries.syncRound(s.storage, r.currentHeight, r.currentRound) match
        case Some(r2) => s.set(enterRound(r, r2)) // lines 24 to 26
        case None =>
          r.currentStep match // lines 29 to 33
            case Step.PROPOSAL => computeProposal(s)
            case Step.PREVOTE => computePrevote(s)
            case Step.PRECOMMIT => computePrecommit(s)

  private def computeProposal(s: S): S =
    val r = s.registers
    leaderProposal(s) match // lines 36 to 38
      case None => s // line 61
      case Some(p) =>
        val b = p.block // line 40
        val j = p.metadata.justification // line 41
        val vr = roundOf(j) // line 42
        val lr = roundOf(r.lockedQC) // line 43
        val lv = r.lockedQC.flatMap(_.value) // line 44
        val locked = r.lockedQC.isDefined
        val justified = verifyPrevoteQC(j, r.currentHeight, r.currentRound)
        val x =
          if justified && j.exists(_.value == b) && vr >= lr && valid(b)
          then b // lines 47 to 49
          else if !valid(b) || (locked && lr > vr && lv != b)
          then None // lines 52 to 53
          else if valid(b) && (!locked || lv == b) then b // lines 56 to 57
          else None // line 60
        s.set(r.copy(proposal = x, currentStep = Step.PREVOTE))

  private def computePrevote(s: S): S =
    val r = s.registers
    val (h, rd) = (r.currentHeight, r.currentRound)
    val qc = queries.evalQC(s.storage, h, rd, Step.PREVOTE) // line 65
    val b = qc.flatMap(_.value) // line 66
    if valid(b) && proposed(s) == b then // lines 68 to 73
      s.set(r.copy(
        lockedQC = qc,
        validQC = qc,
        vote = b,
        currentStep = Step.PRECOMMIT
      ))
    else if queries.hasNilQC(s.storage, h, rd, Step.PREVOTE) then // 76 to 77
      s.set(r.copy(vote = None, currentStep = Step.PRECOMMIT))
    else s // line 81

  private def computePrecommit(s: S): S =
    val r = s.registers
    val (h, rd) = (r.currentHeight, r.currentRound)
    val qcPrevote = queries.evalQC(s.storage, h, rd, Step.PREVOTE) // line 87
    val bv = qcPrevote.flatMap(_.value) // line 88
    val r1 = // lines 89 to 93
      if valid(bv) && proposed(s) == bv then r.copy(validQC = qcPrevote)
      else r
    val d = // lines 96 to 100
      for
        qc <- queries.highestQC(s.storage, h, rd, Step.PRECOMMIT)
        b <- qc.value if app.isValid(b) && !r1.decisions.contains(h)
      yield Decision(b, qc)
    val r2 = d.fold(r1)(d => r1.copy(decisions = r1.decisions.updated(h, d)))
    if r2.decisions.contains(h) then s.set(nextHeight(r2)) // lines 103 to 104
    else if queries.hasNilQC(s.storage, h, rd, Step.PRECOMMIT) then // 107
      s.set(enterRound(r2, rd + 1))
    else s.set(r2) // line 112

  /** Lines 2 to 5: the quorum check belongs to Layer 3. */
  private def verifyQC(qc: Option[QC]): Boolean = qc.exists(queries.verifyQC)

  /** Lines 9 to 15: a prevote certificate at height h and a round below r. */
  private def verifyPrevoteQC(qc: Option[QC], h: Long, r: Long): Boolean =
    val at = qc.exists(q =>
      q.slot.height == h && q.slot.round < r && q.slot.step == Step.PREVOTE
    )
    at && verifyQC(qc)

  /** Round 0 of height h, the height-scoped registers cleared. */
  private def fresh(d: Map[Long, Decision[E]], h: Long): R =
    val p = Some(app.getValue(h, 0L))
    Registers(d, h, 0L, Step.PROPOSAL, None, None, p, None)

  private def nextHeight(r: R): R = fresh(r.decisions, r.currentHeight + 1)

  /** Round rd of the current height, re-seeding the proposal from validQC. */
  private def enterRound(r: R, rd: Long): R =
    val p = r.validQC match
      case Some(qc) => qc.value
      case None => Some(app.getValue(r.currentHeight, rd))
    r.copy(
      currentRound = rd,
      currentStep = Step.PROPOSAL,
      proposal = p,
      vote = None
    )

  /** The body read at the leader's proposal slot of the current round. */
  private def leaderProposal(s: S): Option[Body[TmMeta[E]]] =
    val r = s.registers
    val (h, rd) = (r.currentHeight, r.currentRound)
    val w = Slot(h, rd, Step.PROPOSAL, app.getProposer(h, rd))
    queries.readSlot(s.storage, w).map(_._1)

  private def proposed(s: S): Option[Block] = leaderProposal(s).flatMap(_.block)

  private def valid(b: Option[Block]): Boolean = b.exists(app.isValid)

  /** The round of a certificate, −1 standing for ⊥. */
  private def roundOf(qc: Option[QC]): Long = qc.fold(-1L)(_.slot.round)

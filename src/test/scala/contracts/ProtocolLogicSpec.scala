package contracts

import crypto.Evidence
import domain.*
import layers.*
import harness.*
import org.scalatest.funsuite.AnyFunSuite
import java.security.PublicKey

/** Contract K_4 of the protocol logic: the observation after Definition
  * (Execution), Lemma (Lock Invariant), Theorem (Lock Safety), Corollary
  * (Protocol Safety) and Corollary (Protocol Progress).
  *
  * Executions come from simulations at the least N of each trust model (Θ_std
  * at N = 3f + 1, Θ_tee at N = 2f + 1, f = 1 and 2): correct replicas run
  * Tendermint under the engine, over a network that reorders and duplicates,
  * with f Byzantine validators that equivocate, propose invalid blocks, forge
  * justifications and relay certificates. Where a lemma needs a specific shape
  * of execution, it is also checked on a hand-written one.
  */
class ProtocolLogicSpec extends AnyFunSuite:

  private val Runs = 6
  private val Steps = 1500

  /** The simulations of one configuration, computed once and shared. */
  private final class Simulation[E <: Evidence](val c: Configuration[E]):
    lazy val clusters: Vector[Cluster[E]] =
      Vector.tabulate(Runs)(seed => Cluster.simulate(c, seed.toLong, Steps))

    def executions: Vector[(Cluster[E], PublicKey)] =
      for cl <- clusters; v <- cl.replicas yield (cl, v)

  private val std =
    Vector(1, 2).map(Configuration.minimalStd).map(Simulation(_))
  private val tee =
    Vector(1, 2).map(Configuration.minimalTee).map(Simulation(_))

  private def ctx[E <: Evidence](s: ReplicaState[E]): Context = Drive.context(s)

  /** Whether a transition moved the context. */
  private def moved[E <: Evidence](t: Transition[E]): Boolean =
    ctx(t.after) != ctx(t.before)

  // Safety

  /** Observation after Definition (Execution): along an execution the context
    * never decreases, and within one context proposal, vote and lockedQC do not
    * change.
    */
  private def contextAscent[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions do
      val states = cl.statesOf(v)
      for (a, b) <- states.zip(states.tail) do
        val (ra, rb) = (a.registers, b.registers)
        assert(ctx(b) >= ctx(a), s"context went from ${ctx(a)} to ${ctx(b)}")
        if ctx(b) == ctx(a) then
          assert(rb.proposal == ra.proposal, s"proposal changed in ${ctx(a)}")
          assert(rb.vote == ra.vote, s"vote changed in ${ctx(a)}")
          assert(rb.lockedQC == ra.lockedQC, s"lockedQC changed in ${ctx(a)}")

  /** Lemma (Lock Invariant) (1): a state at (h, r, PRECOMMIT) whose vote is a
    * block b was entered by line 73, with lockedQC set to the certificate for b
    * that evalQC returned at (h, r, PREVOTE).
    */
  private def lockInvariantPrecommit[E <: Evidence](sim: Simulation[E]): Unit =
    var checked = 0
    for (cl, v) <- sim.executions; t <- cl.transitionsOf(v) do
      val a = t.after
      val r = a.registers
      val votedBlock = r.currentStep == Step.PRECOMMIT && r.vote.isDefined
      val Context(h, rd, _) = ctx(a)
      if moved(t) && votedBlock then
        checked += 1
        val evaluated =
          sim.c.queries.evalQC(t.before.storage, h, rd, Step.PREVOTE)
        assert(t.kind == Kind.Compute[E](), s"${ctx(a)} entered by ${t.kind}")
        assert(ctx(t.before) == Context(h, rd, Step.PREVOTE))
        assert(r.lockedQC == evaluated, s"${ctx(a)}")
        assert(r.lockedQC.exists(_.value == r.vote), s"${ctx(a)}")
      if votedBlock then
        val lockSlot = CollapsedSlot(h, rd, Step.PREVOTE)
        assert(r.lockedQC.exists(q => q.slot == lockSlot && q.value == r.vote))
    assert(checked > 0, "no replica precommitted a block")

  /** Lemma (Lock Invariant) (2): a state at (h, r', PREVOTE) whose proposal is
    * a block b', locked on another block at round lr, was entered by line 49,
    * on a justification for b' that passed VerifyPrevoteQC at a round vr with
    * lr ≤ vr < r'.
    */
  private def lockInvariantPrevote[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions; t <- cl.transitionsOf(v) do
      val a = t.after
      val r = a.registers
      val lockedOther = r.lockedQC.exists(_.value != r.proposal)
      val prevoted = r.currentStep == Step.PREVOTE && r.proposal.isDefined
      if moved(t) && prevoted && lockedOther then
        val Context(h, r2, _) = ctx(a)
        val lr = r.lockedQC.get.slot.round
        val entered = ctx(t.before) == Context(h, r2, Step.PROPOSAL)
        assert(t.kind == Kind.Compute[E]() && entered, s"${ctx(a)}")
        val leaderSlot = Slot(h, r2, Step.PROPOSAL, cl.proposer(h, r2))
        val justification = sim.c.queries
          .readSlot(t.before.storage, leaderSlot)
          .flatMap(_._1.metadata.justification)
        assert(justification.isDefined, s"${ctx(a)}: prevoted unjustified")
        val j = justification.get
        val atPrevote = j.slot.height == h && j.slot.step == Step.PREVOTE
        assert(atPrevote && sim.c.queries.verifyQC(j), s"${ctx(a)}")
        assert(
          lr <= j.slot.round && j.slot.round < r2,
          s"${ctx(a)}: vr = ${j.slot.round}, lr = $lr"
        )
        assert(j.value == r.proposal, s"${ctx(a)}")

  /** Every grounded certificate of a simulation: those evalQC returns on any
    * storage of any replica (highestQC returns only these), and every admitted
    * certificate or justification that passes verifyQC.
    */
  private def grounded[E <: Evidence](cl: Cluster[E]): Vector[TmQC[E]] =
    Execution(0L, cl).certificates

  /** Theorem (Lock Safety), under an admissible configuration: a grounded
    * precommit certificate for b at (h, r) and a grounded prevote certificate
    * for b' at (h, r') with r' > r have b' = b.
    */
  private def lockSafety[E <: Evidence](sim: Simulation[E]): Unit =
    assert(sim.c.queries.admissible)
    var pairs = 0
    for cl <- sim.clusters do
      val qcs = grounded(cl).filter(_.value.isDefined)
      for
        pc <- qcs if pc.slot.step == Step.PRECOMMIT
        pv <- qcs if pv.slot.step == Step.PREVOTE
        if pv.slot.height == pc.slot.height && pv.slot.round > pc.slot.round
      do
        pairs += 1
        assert(pv.value == pc.value, s"precommit $pc, prevote $pv")
    info(s"$pairs pairs of a precommit and a later prevote certificate checked")

  /** Remark (Lock Safety under Θ_tee): Θ_tee is not admissible, and without the
    * certified-history rule, which the architecture does not represent, a
    * Byzantine validator shared by two quorums need not respect its lock. With
    * N = 3 and f = 1, one correct replica decides x at round 0 and the other
    * decides y at round 1: Agreement holds only in its same-round form.
    */
  private def teeCrossRoundDisagreement(): Unit =
    val c = Configuration.minimalTee(1)
    val members = c.validators.members
    val (byz, v1, v2) = (members(0), members(1), members(2))
    assert(c.byzantine(byz) && !c.queries.admissible)

    val rt = TeeRuntime()
    val proposer = (_: Long, _: Long) => byz
    val p1 = Tendermint(v1, c.queries, TestApplication(v1, members, proposer))
    val p2 = Tendermint(v2, c.queries, TestApplication(v2, members, proposer))
    val x = Some(TestApplication.block("x", 0L))
    val y = Some(TestApplication.block("y", 0L))

    // the Byzantine enclave signs round 0, then round 1, one counter per step
    def byzantine(r: Long, x: Option[Block]) =
      Step.values.toVector.map(s => Drive.vote(rt, byz, 0L, r, s, x))
    val Vector(proposeX, prevoteX, precommitX) = byzantine(0, x): @unchecked
    val Vector(proposeY, prevoteY, precommitY) = byzantine(1, y): @unchecked

    // v1 sees round 0 only, and decides x
    var s1 = Drive.deliver(p1, p1.init(), proposeX)
    s1 = Drive.deliver(p1, s1, (prevoteX +: Drive.signSend(p1, s1, rt))*)
    s1 = Drive.deliver(p1, s1, (precommitX +: Drive.signSend(p1, s1, rt))*)
    assert(s1.registers.decisions.get(0L).map(_.block) == x)

    // v2 times out of round 0, admits the Byzantine round 0 in counter order
    // and decides y
    var s2 = p2.onTimeout(p2.init(), Context(0, 0, Step.PROPOSAL))
    val nilPrevote = Drive.signSend(p2, s2, rt)
    s2 = p2.onTimeout(s2, Context(0, 0, Step.PREVOTE))
    val nilPrecommit = Drive.signSend(p2, s2, rt)
    s2 = p2.onTimeout(s2, Context(0, 0, Step.PRECOMMIT))
    val round0 = Vector(proposeX, prevoteX, precommitX)
    s2 = Drive.deliver(p2, s2, (round0 ++ nilPrevote ++ nilPrecommit)*)
    assert(ctx(s2) == Context(0, 1, Step.PROPOSAL))
    s2 = Drive.deliver(p2, s2, proposeY)
    s2 = Drive.deliver(p2, s2, (prevoteY +: Drive.signSend(p2, s2, rt))*)
    s2 = Drive.deliver(p2, s2, (precommitY +: Drive.signSend(p2, s2, rt))*)
    assert(s2.registers.decisions.get(0L).map(_.block) == y)

    val qc1 = s1.registers.decisions(0L).qc
    val qc2 = s2.registers.decisions(0L).qc
    assert(qc1.slot.round != qc2.slot.round)
    val shared =
      qc1.proof.validators.toSet.intersect(qc2.proof.validators.toSet)
    assert(shared == Set(byz))

  /** Theorem (Agreement): decisions recorded at h with certificates of one
    * round are the same block; under an admissible configuration all decisions
    * at h are.
    */
  private def agreement[E <: Evidence](sim: Simulation[E]): Unit =
    var decided = 0
    for cl <- sim.clusters do
      val decisions = Execution(0L, cl).decisions
      decided += decisions.size
      for (h, atH) <- decisions.groupBy(_._1) do
        for (r, atR) <- atH.groupBy(_._2) do
          val blocks = atR.map(_._3.blockId).distinct
          assert(blocks.size == 1, s"height $h, round $r: $blocks")
        if sim.c.queries.admissible then
          val blocks = atH.map(d => (d._2, d._3.blockId))
          assert(atH.map(_._3).distinct.size == 1, s"height $h: $blocks")
    assert(decided > 0, "no replica decided")

  /** Theorem (Integrity): once an execution records decisions[h], the entry
    * never changes.
    */
  private def integrity[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions do
      val states = cl.statesOf(v)
      for (a, b) <- states.zip(states.tail); (h, d) <- a.registers.decisions do
        assert(b.registers.decisions.get(h).contains(d), s"decisions[$h]")

  // Progress

  /** Lemma (Finite Ascent): Compute iterated on any state of an execution stops
    * moving the context, and once Compute keeps the context of S it keeps that
    * of Compute(S).
    */
  private def finiteAscent[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions do
      val p = cl.freshProtocol(v)
      for (s, i) <- cl.statesOf(v).zipWithIndex if i % 5 == 0 do
        val fixpoint = Drive.settle(p, s, bound = 1000)
        assert(p.context(p.compute(fixpoint)) == p.context(fixpoint))
        if p.context(p.compute(s)) == p.context(s) then
          val twice = p.context(p.compute(p.compute(s)))
          assert(twice == p.context(s), s"${ctx(s)}")

  /** Lemma (Timeout Advance): a timeout stamped with the current context moves
    * it forward, and one stamped with any other context changes nothing.
    */
  private def timeoutAdvance[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions do
      val p = cl.freshProtocol(v)
      for s <- cl.statesOf(v) do
        val Context(h, r, step) = ctx(s)
        val nextStep = Step.fromOrdinal((step.ordinal + 1) % 3)
        assert(p.context(p.onTimeout(s, ctx(s))) > ctx(s), s"${ctx(s)}")
        for
          stale <- Vector(
            Context(h, r + 1, step),
            Context(h + 1, r, step),
            Context(h, r, nextStep)
          )
        do assert(p.onTimeout(s, stale) == s, s"${ctx(s)}: stale $stale acted")

  /** Lemma (Decisive Evidence), first sentence, on executions: every round is
    * entered at PROPOSAL, and there the Send of the leader contains its
    * proposal.
    */
  private def roundsEnteredAtProposal[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions do
      val p = cl.freshProtocol(v)
      val states = cl.statesOf(v)
      for (a, b) <- states.zip(states.tail) do
        val (ca, cb) = (ctx(a), ctx(b))
        if ca.height != cb.height || ca.round != cb.round then
          assert(cb.step == Step.PROPOSAL, s"entered $cb")
      for s <- states do
        val Context(h, r, step) = ctx(s)
        if step == Step.PROPOSAL && cl.proposer(h, r) == v then
          val own = Slot(h, r, Step.PROPOSAL, v)
          val sent = p.send(s).collect { case Outbound.Payload(w, _) => w }
          assert(sent.contains(own), s"${ctx(s)}")

  /** Lemma (Decisive Evidence) (1) to (3), on a well-led round: a correct
    * leader proposes a valid block b, and a Byzantine validator prevotes
    * another block and nil and precommits another block. The replica keeps its
    * state until the proposal, prevotes b, keeps the context until it holds
    * every correct prevote, precommits b, keeps it until it holds every correct
    * precommit, and decides b. A second round checks the justified case: locked
    * on b at round 0, the replica prevotes b at round 1 on a proposal justified
    * by the prevote certificate of round 0.
    */
  private def decisiveEvidence[E <: Evidence](c: Configuration[E]): Unit =
    val rt = c.newRuntime()
    val members = c.validators.members
    val correct = members.filter(c.correct)
    val byz = members.filter(c.byzantine).head
    val (leader, self) = (correct(0), correct(1))
    val proposer = (_: Long, _: Long) => leader
    def replica(v: PublicKey) =
      Tendermint(v, c.queries, TestApplication(v, members, proposer))
    val (p, leading) = (replica(self), replica(leader))
    val other = Some(TestApplication.block("other", 0L))
    def votes(step: Step, x: Option[Block], vs: Vector[PublicKey]) =
      vs.map(v => Drive.vote(rt, v, 0, 0, step, x))
    def payload(step: Step, x: Option[Block]): Outbound[E] =
      Outbound.Payload(Slot(0, 0, step, self), Body(x, TmMeta.empty))

    val byzantine = votes(Step.PREVOTE, other, Vector(byz)) ++
      votes(Step.PREVOTE, None, Vector(byz)) ++
      votes(Step.PRECOMMIT, other, Vector(byz))
    var s = Drive.ingest(p.init(), byzantine*)
    assert(p.compute(s) == s, "(1) moved without the proposal")

    val proposal = Drive.signSend(leading, leading.init(), rt)
    val leaderSlot = Slot(0, 0, Step.PROPOSAL, leader)
    assert(proposal.exists(_.slot == leaderSlot), "the leader did not propose")
    val b = proposal.head.body.block
    assert(b.exists(TestApplication(self, members, proposer).isValid))

    s = Drive.deliver(p, s, proposal*)
    assert(ctx(s) == Context(0, 0, Step.PREVOTE), "(1)")
    assert(p.send(s) == Set(payload(Step.PREVOTE, b)), "(1) no prevote for b")

    val prevotes = votes(Step.PREVOTE, b, correct)
    s = Drive.deliver(p, s, prevotes.init*)
    assert(ctx(s) == Context(0, 0, Step.PREVOTE), "(2) moved too early")
    s = Drive.deliver(p, s, prevotes.last)
    assert(ctx(s) == Context(0, 0, Step.PRECOMMIT), "(2) did not move")
    val precommit = Set(payload(Step.PRECOMMIT, b))
    assert(p.send(s) == precommit, "(2) no precommit for b")
    val locked = s

    val precommits = votes(Step.PRECOMMIT, b, correct)
    s = Drive.deliver(p, s, precommits.init*)
    val waiting = ctx(s) == Context(0, 0, Step.PRECOMMIT)
    assert(waiting && s.registers.decisions.isEmpty, "(3) decided too early")
    s = Drive.deliver(p, s, precommits.last)
    val decided = s.registers.decisions.get(0L).map(_.block)
    assert(decided == b, "(3) did not decide b")
    assert(ctx(s) == Context(1, 0, Step.PROPOSAL))

    // justified case, from the locked state
    var t = p.onTimeout(locked, ctx(locked))
    val atRound1 = ctx(t) == Context(0, 1, Step.PROPOSAL)
    assert(atRound1 && t.registers.lockedQC.isDefined)
    val justification = c.queries.evalQC(locked.storage, 0, 0, Step.PREVOTE)
    val justified =
      rt.sign(Slot(0, 1, Step.PROPOSAL, leader), Body(b, TmMeta(justification)))
    t = Drive.deliver(p, t, justified)
    val prevotedB = ctx(t) == Context(0, 1, Step.PREVOTE)
    assert(prevotedB && t.registers.proposal == b, "justified not prevoted")

  /** Lemma (Decision Relay) (1): no decision without a move, heights advance
    * one at a time into (h + 1, 0, PROPOSAL) and only past a decision; (2)
    * there Send announces the decision of h; (3) FastForward records every
    * admitted precommit certificate for a valid block that passes verifyQC at
    * an undecided height.
    */
  private def decisionRelay[E <: Evidence](sim: Simulation[E]): Unit =
    var fastForwards = 0
    for (cl, v) <- sim.executions do
      val p = cl.freshProtocol(v)
      val transitions = cl.transitionsOf(v)
      for (t, next) <- transitions.zip(transitions.tail) do
        if t.after != next.before then
          assert(
            next.before.registers == t.after.registers,
            "ingestion moved R"
          )
      for t <- transitions do
        val (a, b) = (t.before.registers, t.after.registers)
        val h0 = a.currentHeight
        t.kind match
          case Kind.OnTimeout(_) =>
            assert(b.decisions == a.decisions, "OnTimeout recorded a decision")
          case Kind.Compute() if a.decisions.contains(h0) =>
            assert(moved(t), "Compute kept a decided height")
          case Kind.FastForward(qc) =>
            val h = qc.slot.height
            val adoptable = qc.slot.step == Step.PRECOMMIT &&
              qc.value.exists(cl.isValid) && sim.c.queries.verifyQC(qc)
            if adoptable && !a.decisions.contains(h) then
              fastForwards += 1
              val recorded = b.decisions.get(h)
              assert(recorded == qc.value.map(Decision(_, qc)), s"height $h")
          case _ => ()
        if b.decisions.contains(h0) && !a.decisions.contains(h0) then
          assert(moved(t), s"${t.kind} decided height $h0 without moving")
        if b.currentHeight != h0 then
          val next = b.currentHeight == h0 + 1 && b.decisions.contains(h0)
          assert(next, s"height $h0 to ${b.currentHeight}")
          assert(ctx(t.after) == Context(h0 + 1, 0, Step.PROPOSAL))
      for s <- cl.statesOf(v) do
        val Context(h, r, step) = ctx(s)
        if step == Step.PROPOSAL && r == 0 && h > 0 then
          val announced = s.registers.decisions.get(h - 1)
            .map(d => Outbound.Certificate[E](d.qc))
          assert(announced.exists(p.send(s).contains), s"${ctx(s)} silent")
    assert(fastForwards > 0, "no certificate was fast-forwarded")

  // registration

  private def safety[E <: Evidence](sim: Simulation[E]): Unit =
    val n = sim.c.name
    test(s"K4, Definition (Execution) [$n]: contexts never decrease")(
      contextAscent(sim)
    )
    test(s"K4, Lemma (Lock Invariant) (1) [$n]: precommit follows a lock")(
      lockInvariantPrecommit(sim)
    )
    test(s"K4, Lemma (Lock Invariant) (2) [$n]: unlocking is justified")(
      lockInvariantPrevote(sim)
    )
    test(s"K4 Thm, Theorem (Agreement) [$n]: decisions agree")(
      agreement(sim)
    )
    test(s"K4 Thm, Theorem (Integrity) [$n]: decisions never change")(
      integrity(sim)
    )
    test(s"K4 Thm, Lemma (Finite Ascent) [$n]: Compute stops moving")(
      finiteAscent(sim)
    )
    test(s"K4 Thm, Lemma (Timeout Advance) [$n]: stale tokens do nothing")(
      timeoutAdvance(sim)
    )
    test(s"K4 Thm, Lemma (Decisive Evidence) [$n]: rounds start at PROPOSAL")(
      roundsEnteredAtProposal(sim)
    )
    test(s"K4 Thm, Lemma (Decisive Evidence) [$n]: a well-led round decides")(
      decisiveEvidence(sim.c)
    )
    test(s"K4 Thm, Lemma (Decision Relay) [$n]: decisions are relayed")(
      decisionRelay(sim)
    )

  std.foreach(safety)
  tee.foreach(safety)

  for sim <- std do
    test(s"K4, Theorem (Lock Safety) [${sim.c.name}]: later prevotes agree")(
      lockSafety(sim)
    )

  test("Remark (Lock Safety under Θ_tee): two rounds may decide differently")(
    teeCrossRoundDisagreement()
  )

package harness

import crypto.TeeEvidence
import domain.*
import layers.*

import java.security.PublicKey

/** Two decisions at one height, the prevote certificate that justified the
  * second, and the validators that were Byzantine.
  */
final case class Split(
    first: Decision[TeeEvidence],
    second: Decision[TeeEvidence],
    prevote: TmQC[TeeEvidence],
    byzantine: Set[PublicKey]
)

/** Remark (Lock Safety under Θ_tee) as an attack at N = 2f + 1. The f Byzantine
  * validators, one of them the leader of rounds 0 and 1, sign x throughout
  * round 0 and y throughout round 1, each in counter order. The correct v1 sees
  * round 0 only and decides x with them; the correct v2 times out of round 0,
  * admits it, and decides y with them at round 1.
  */
object TeeSplit:
  def apply(c: Configuration[TeeEvidence], x: Block, y: Block): Split =
    val members = c.validators.members
    val byz = members.filter(c.byzantine)
    val Vector(v1, v2) = members.filter(c.correct).take(2): @unchecked
    val rt = c.newRuntime()
    def replica(v: PublicKey) =
      Tendermint(v, c.queries, TestApplication(v, members, (_, _) => byz.head))
    def signed(r: Long, b: Block, s: Step, vs: Vector[PublicKey]) =
      vs.map(v => Drive.vote(rt, v, 0, r, s, Some(b)))
    def round(r: Long, b: Block) = Vector(
      signed(r, b, Step.PROPOSAL, byz.take(1)),
      signed(r, b, Step.PREVOTE, byz),
      signed(r, b, Step.PRECOMMIT, byz)
    )
    val Vector(propX, prevX, precX) = round(0, x): @unchecked
    val Vector(propY, prevY, precY) = round(1, y): @unchecked

    /** Delivers the proposal, then each vote step with the replica's own. */
    def decide(
        p: Tendermint[TeeEvidence],
        s: ReplicaState[TeeEvidence],
        prop: Vector[TmMessage[TeeEvidence]],
        votes: Vector[Vector[TmMessage[TeeEvidence]]]
    ) =
      votes.foldLeft(Drive.deliver(p, s, prop*)) { (st, vs) =>
        Drive.deliver(p, st, (vs ++ Drive.signSend(p, st, rt))*)
      }

    val p1 = replica(v1)
    val s1 = decide(p1, p1.init(), propX, Vector(prevX, precX))

    val p2 = replica(v2)
    val timeouts = Vector(Step.PROPOSAL, Step.PREVOTE, Step.PRECOMMIT)
    val (s2, nil) =
      timeouts.foldLeft((p2.init(), Vector.empty[TmMessage[TeeEvidence]])) {
        case ((st, out), step) =>
          val next = p2.onTimeout(st, Context(0, 0, step))
          (
            next,
            if step == Step.PRECOMMIT then out
            else out ++ Drive.signSend(p2, next, rt)
          )
      }
    val seen = Drive.deliver(p2, s2, (propX ++ prevX ++ precX ++ nil)*)
    val t2 = decide(p2, seen, propY, Vector(prevY, precY))
    val prevote = c.queries.evalQC(t2.storage, 0, 1, Step.PREVOTE).get
    Split(
      s1.registers.decisions(0),
      t2.registers.decisions(0),
      prevote,
      byz.toSet
    )

package harness

import crypto.{Evidence, SoftwareEvidence, TeeEvidence}
import domain.*
import layers.*
import org.scalacheck.Gen

import scala.util.Random

/** The adversarial harness, a black box for the layer properties. Every
  * generator draws a seed from ScalaCheck and replays the system model from it:
  * noisy histories with Byzantine equivocation, admission, reordering and
  * duplicated delivery, and whole executions over an asynchronous network.
  */
object AdversarialHarness:

  type Msg[E <: Evidence] = WireMessage[String, E]
  type State[E <: Evidence] = Accumulation[String, E]

  /** Θ_std at N = 4 and Θ_tee at N = 3, f = 1, where quorums are tightest. */
  val std: Configuration[SoftwareEvidence] = Configuration.minimalStd(1)
  val tee: Configuration[TeeEvidence] = Configuration.minimalTee(1)

  private val seed: Gen[Random] = Gen.long.map(new Random(_))

  /** The admitted part of a noisy history, reordered and duplicated. */
  def genAdversarialBatch[E <: Evidence](c: Configuration[E]) =
    seed.map(r => c.gen.replica(r, c.gen.history(r, 20 + r.nextInt(80)), 20))

  /** Two deliveries of one admitted set, in different orders and with different
    * duplicates.
    */
  def genSchedules[E <: Evidence](
      c: Configuration[E]
  ): Gen[(Vector[Msg[E]], Vector[Msg[E]])] =
    seed.map { r =>
      val set = c.gen.admit(r, c.gen.history(r, 20 + r.nextInt(80)))
      (c.gen.delivery(r, set, r.nextInt(20)), c.gen.delivery(r, set, 20))
    }

  /** A slot where a Byzantine validator equivocated, the first message it
    * signed there, and a replica state that admitted two or more of them.
    */
  def genEquivocation[E <: Evidence](
      c: Configuration[E]
  ): Gen[(Slot, Msg[E], State[E])] =
    seed.map { r =>
      val history = c.gen.votingHistory(r)
      val l = Accumulation.of(c.gen.replica(r, history, 10))
      l.slots.collectFirst {
        case w if l(w).sizeIs > 1 =>
          (w, history.find(_.slot == w).get, l)
      }
    }.suchThat(_.isDefined).map(_.get)

  /** Two replica states of one admitted voting history, reached by different
    * deliveries.
    */
  def genVotingPair[E <: Evidence](
      c: Configuration[E]
  ): Gen[(State[E], State[E])] =
    seed.map { r =>
      val set = c.gen.admit(r, c.gen.votingHistory(r))
      def state() = Accumulation.of(c.gen.delivery(r, set, r.nextInt(10)))
      (state(), state())
    }

  /** Every step coordinate of the voting domain, one height and round beyond.
    */
  def coordinates[E <: Evidence](c: Configuration[E]): Vector[CollapsedSlot] =
    for
      h <- (0L to c.gen.domain.heights).toVector
      r <- 0L to c.gen.domain.rounds
      s <- Step.values.toVector
    yield CollapsedSlot(h, r, s)

  /** A whole execution: correct replicas under the engine, f Byzantine. */
  def genExecution[E <: Evidence](c: Configuration[E]): Gen[Execution[E]] =
    Gen.long.map(s => Execution(s, Cluster.simulate(c, s, steps = 1500)))

  /** The split attack of Remark (Lock Safety under Θ_tee), with f = 1 or 2. */
  def genTeeSplit: Gen[Split] =
    for
      c <- Gen.oneOf(tee, teeWide)
      (i, j) <- Gen.zip(Gen.posNum[Int], Gen.posNum[Int])
    yield TeeSplit(
      c,
      TestApplication.block(s"x$i", 0),
      TestApplication.block(s"y$j", 0)
    )

  private lazy val teeWide = Configuration.minimalTee(2)

/** What an execution exposes to the properties of Layers 4 and 5. */
final class Execution[E <: Evidence](seed: Long, cl: Cluster[E]):

  private val states = cl.replicas.flatMap(cl.statesOf)

  /** Grounded certificates: those evalQC returns on any storage of any replica,
    * and every admitted certificate or justification that verifies.
    */
  lazy val certificates: Vector[TmQC[E]] =
    val q = cl.config.queries
    val returned =
      for
        l <- states.map(_.storage).distinct
        (h, r) <- l.slots.map(w => (w.height, w.round)).toVector.distinct
        s <- Vector(Step.PREVOTE, Step.PRECOMMIT)
        qc <- q.evalQC(l, h, r, s)
      yield qc
    val relayed = cl.admittedCertificates.toVector ++
      cl.replicas.flatMap(cl.admitted).flatMap(_.body.metadata.justification)
    (returned ++ relayed.filter(q.verifyQC)).distinct

  /** Every decision (h, round, block) held in any state of any replica. */
  lazy val decisions: Vector[(Long, Long, Block)] =
    val all =
      for s <- states; (h, d) <- s.registers.decisions
      yield (h, d.qc.slot.round, d.block)
    all.distinct

  /** Per replica, a fresh protocol to probe its states, its states and its
    * Layer 4 transitions, in order.
    */
  lazy val runs: Vector[Run[E]] =
    cl.replicas.map(v =>
      Run(cl.freshProtocol(v), cl.statesOf(v), cl.transitionsOf(v))
    )

  def queries: QuorumQueries[E] = cl.config.queries

  /** Per replica, a fresh engine and the messages it admitted, in order. */
  lazy val deliveries: Vector[(StabilizationEngine[E], Vector[TmMessage[E]])] =
    cl.replicas.map(v =>
      (StabilizationEngine(cl.freshProtocol(v)), cl.admitted(v).toVector)
    )

  /** Whether every correct replica decided at least one height. */
  def everyoneDecided: Boolean =
    cl.replicas.forall(v => cl.state(v).registers.ledger.nonEmpty)

  /** Per replica, its committed ledger after every call of F. */
  lazy val ledgers: Vector[Vector[Vector[Block]]] =
    cl.replicas.map(v => cl.calls(v).map(_.after.registers.ledger).toVector)

  override def toString: String = s"Execution(${cl.config.name}, seed $seed)"

final case class Run[E <: Evidence](
    p: Tendermint[E],
    states: Vector[ReplicaState[E]],
    transitions: Vector[Transition[E]]
)

package harness

import crypto.{Evidence, SoftwareEvidence, TeeEvidence}
import domain.*
import layers.Accumulation
import org.scalatest.Assertions.withClue
import scodec.bits.ByteVector
import java.security.PublicKey
import scala.util.Random

/** Sizes of the value domains. Small, so that many messages fall on one slot
  * and Byzantine validators equivocate often.
  */
final case class Domain(
    heights: Int = 3,
    rounds: Int = 3,
    blocks: Int = 3,
    metadata: Vector[String] = Vector("a", "b"),
    signatures: Int = 3
)

object Domain:
  /** Few candidates per coordinate, so quorums and conflicts are frequent. */
  val voting: Domain = Domain(heights = 2, rounds = 3, blocks = 2)

/** What a validator decides to sign, before its proof exists. */
final case class Draft(slot: Slot, body: Body[String], signature: Int):
  def sig: ByteVector = ByteVector(signature.toByte)

/** Random executions of the system model, in three steps:
  *   1. `history`: every message signed, in signing order. A correct validator
  *      signs at most one message per slot, a Byzantine one may sign several
  *      (equivocation).
  *   2. `admit`: the subset of the history one replica admits.
  *   3. `delivery`: an admissible order of that set, with re-deliveries.
  * Subclasses fix the proof, the admissible subsets and the orders.
  */
trait MessageGen[E <: Evidence]:
  type Msg = WireMessage[String, E]

  def validators: Vector[PublicKey]
  def byzantine: Set[PublicKey]
  def domain: Domain

  protected def sign(drafts: Vector[Draft]): Vector[Msg]
  def admit(rnd: Random, history: Vector[Msg]): Vector[Msg]
  def schedule(rnd: Random, admitted: Vector[Msg]): Vector[Msg]

  /** The messages admission accepts as entries of a certificate. */
  def certifiable(history: Vector[Msg]): Vector[Msg]

  final def correct: Vector[PublicKey] = validators.filterNot(byzantine)

  final def history(rnd: Random, n: Int): Vector[Msg] = sign(drafts(rnd, n))

  /** A history that looks like voting: one favoured candidate per step
    * coordinate (nil included); each correct validator votes once with
    * probability `participation`, for the favoured one with probability
    * `agreement`; each Byzantine one signs zero to two messages there.
    */
  final def votingHistory(
      rnd: Random,
      participation: Double = 0.9,
      agreement: Double = 0.85
  ): Vector[Msg] =
    sign(rnd.shuffle(votingDrafts(rnd, participation, agreement)))

  final def pair(m: Msg): (Body[String], E) = (m.body, m.evidence)

  final def slotsOf(ms: Vector[Msg]): Vector[Slot] = ms.map(_.slot).distinct

  /** A slot no generated message ever uses. */
  final def unpopulatedSlot: Slot =
    Slot(domain.heights + 1000L, 0L, Step.PROPOSAL, validators.head)

  /** Re-delivers `k` messages, each after its first delivery. */
  final def withDuplicates(rnd: Random, order: Vector[Msg], k: Int) =
    if order.isEmpty then order
    else
      (1 to k).foldLeft(order) { (acc, _) =>
        val i = rnd.nextInt(acc.size)
        val j = i + 1 + rnd.nextInt(acc.size - i)
        acc.patch(j, Vector(acc(i)), 0)
      }

  final def delivery(rnd: Random, admitted: Vector[Msg], k: Int): Vector[Msg] =
    withDuplicates(rnd, schedule(rnd, admitted), k)

  final def replica(rnd: Random, history: Vector[Msg], k: Int): Vector[Msg] =
    delivery(rnd, admit(rnd, history), k)

  private def pick[T](rnd: Random, xs: IndexedSeq[T]): T =
    xs(rnd.nextInt(xs.size))

  private def anyBlock(rnd: Random, h: Long): Block =
    MessageGen.block(rnd.nextInt(domain.blocks), h)

  private def draft(rnd: Random, w: Slot, x: Option[Block]): Draft =
    val a = pick(rnd, domain.metadata)
    Draft(w, Body(x, a), rnd.nextInt(domain.signatures))

  private def randomDraft(rnd: Random): Draft =
    val h = rnd.nextInt(domain.heights).toLong
    val r = rnd.nextInt(domain.rounds).toLong
    val w = Slot(h, r, pick(rnd, Step.values.toVector), pick(rnd, validators))
    draft(rnd, w, Option.when(rnd.nextBoolean())(anyBlock(rnd, h)))

  private def candidate(rnd: Random, h: Long): Option[Block] =
    Option.when(rnd.nextDouble() < 0.75)(anyBlock(rnd, h))

  private def votingDrafts(rnd: Random, part: Double, agree: Double) =
    for
      h <- (0L until domain.heights).toVector
      r <- 0L until domain.rounds
      s <- Step.values.toVector
      fav = candidate(rnd, h)
      v <- validators
      d <- votes(rnd, Slot(h, r, s, v), fav, part, agree)
    yield d

  private def votes(
      rnd: Random,
      w: Slot,
      fav: Option[Block],
      part: Double,
      agree: Double
  ): Vector[Draft] =
    def vote(x: Option[Block]) = draft(rnd, w, x)
    def other = candidate(rnd, w.height)
    if byzantine(w.validator) then
      Vector.fill(rnd.nextInt(3))(
        vote(if rnd.nextBoolean() then fav else other)
      )
    else if rnd.nextDouble() < part then
      Vector(vote(if rnd.nextDouble() < agree then fav else other))
    else Vector.empty

  /** `n` random drafts, keeping one draft per slot for correct validators. */
  private def drafts(rnd: Random, n: Int): Vector[Draft] =
    val used = collection.mutable.Set.empty[Slot]
    Vector.fill(n)(randomDraft(rnd)).filter { d =>
      byzantine(d.slot.validator) || used.add(d.slot)
    }

object MessageGen:
  /** A block with a one-byte payload. */
  def block(id: Int, height: Long): Block =
    Block(s"block-$id", height, ByteVector(id.toByte))

/** Θ_std: any subset of the history may be admitted, in any order. */
final class SoftwareMessageGen(
    val validators: Vector[PublicKey],
    val byzantine: Set[PublicKey] = Set.empty,
    val domain: Domain = Domain()
) extends MessageGen[SoftwareEvidence]:

  protected def sign(drafts: Vector[Draft]): Vector[Msg] =
    drafts.map(d => WireMessage(d.slot, d.body, SoftwareEvidence(d.sig)))

  def admit(rnd: Random, history: Vector[Msg]): Vector[Msg] =
    history.filter(_ => rnd.nextBoolean())

  def schedule(rnd: Random, admitted: Vector[Msg]): Vector[Msg] =
    rnd.shuffle(admitted)

  /** A certificate entry is admitted on its signature alone. */
  def certifiable(history: Vector[Msg]): Vector[Msg] = history

/** Θ_tee: each enclave keeps one counter per step and attests each position
  * once (Definition Counter Context). Admission follows counter order, so an
  * admitted set holds a prefix of every context.
  */
final class TeeMessageGen(
    val validators: Vector[PublicKey],
    val byzantine: Set[PublicKey] = Set.empty,
    val domain: Domain = Domain()
) extends MessageGen[TeeEvidence]:

  private def context(w: Slot) = (w.validator, w.step)

  /** Counters grow by one per context, in signing order, from 0. */
  protected def sign(drafts: Vector[Draft]): Vector[Msg] =
    val next = collection.mutable.Map.empty[(PublicKey, Step), Long]
    drafts.map { d =>
      val c = next.getOrElse(context(d.slot), 0L)
      next(context(d.slot)) = c + 1
      WireMessage(d.slot, d.body, TeeEvidence(d.sig, c))
    }

  def admit(rnd: Random, history: Vector[Msg]): Vector[Msg] =
    byContext(history).flatMap(seq => seq.take(rnd.nextInt(seq.size + 1)))

  /** A random interleaving that keeps each context in counter order. */
  def schedule(rnd: Random, admitted: Vector[Msg]): Vector[Msg] =
    var queues = byContext(admitted).map(_.toList)
    val out = Vector.newBuilder[Msg]
    while queues.nonEmpty do
      val i = rnd.nextInt(queues.size)
      out += queues(i).head
      val rest = queues(i).tail
      queues =
        if rest.isEmpty then queues.patch(i, Nil, 1)
        else queues.updated(i, rest)
    out.result()

  /** Only the first message of a validator at a slot enters a certificate. */
  def certifiable(history: Vector[Msg]): Vector[Msg] =
    firstMessages(history).values.toVector

  /** The first message at each slot: the one of least counter. */
  def firstMessages(history: Vector[Msg]): Map[Slot, Msg] =
    history.groupMapReduce(_.slot)(identity) { (a, b) =>
      if a.evidence.counter <= b.evidence.counter then a else b
    }

  /** Messages grouped by context in counter order, groups in a fixed order. */
  private def byContext(ms: Vector[Msg]): Vector[Vector[Msg]] =
    ms.groupBy(m => context(m.slot)).values.toVector
      .map(_.sortBy(_.evidence.counter))
      .sortBy(g =>
        (validators.indexOf(g.head.slot.validator), g.head.slot.step.ordinal)
      )

/** How a replica turns a delivery sequence into states. */
object Replica:
  /** Element k is the state after k messages, ⊥ included. */
  def statesAlong[A, E <: Evidence](ms: Seq[WireMessage[A, E]]) =
    ms.toVector.scanLeft(Accumulation.empty[A, E])((l, m) => l.ingest(m))

  /** Each batch folded into a delta Δ_B, the deltas joined. */
  def ingestBatched[A, E <: Evidence](ms: Seq[WireMessage[A, E]], k: Int) =
    ms.grouped(k).map(Accumulation.of).reduceOption(_.merge(_))
      .getOrElse(Accumulation.empty[A, E])

/** A randomized check over a fixed range of seeds, so failures reproduce. */
object Seeds:
  val Default: Int = 200

  def foreach(runs: Int = Default)(check: Random => Unit): Unit =
    for seed <- 0 until runs do
      withClue(s"seed $seed: ")(check(new Random(seed)))

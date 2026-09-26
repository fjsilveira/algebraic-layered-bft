package harness

import blockchain.{StdTrust, TeeTrust, TrustModel, ValidatorsSet}
import crypto.{Evidence, SoftwareEvidence, TeeEvidence}
import domain.*
import layers.*
import scodec.bits.ByteVector
import java.security.spec.NamedParameterSpec
import java.security.{KeyPairGenerator, PublicKey, SecureRandom}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Validator keys from a seeded generator; every call draws fresh keys. */
object TestValidators:
  private val seeds = AtomicLong(1L)

  def generate(n: Int): Vector[PublicKey] =
    val random = SecureRandom.getInstance("SHA1PRNG")
    random.setSeed(seeds.getAndIncrement())
    val kpg = KeyPairGenerator.getInstance("Ed25519")
    kpg.initialize(NamedParameterSpec.ED25519, random)
    Vector.fill(n)(kpg.generateKeyPair().getPublic)

/** Θ = (π, N, Q_size) with a population of exactly f Byzantine validators, a
  * generator of histories, the queries of Layer 3 and a runtime factory.
  */
final case class Configuration[E <: Evidence](
    label: String,
    trust: TrustModel[E],
    gen: MessageGen[E],
    newRuntime: () => Runtime[E]
):
  val validators: ValidatorsSet = ValidatorsSet(gen.validators)
  val n: Int = validators.size
  val f: Int = gen.byzantine.size
  val qSize: Int = trust.quorumSize(n)
  val byzantine: Set[PublicKey] = gen.byzantine
  val correct: Set[PublicKey] = gen.correct.toSet
  val queries: QuorumQueries[E] = QuorumQueries(trust, validators)
  require(f <= trust.maxFaulty(n), s"$label: too many Byzantine validators")

  def name: String = s"$label N=$n f=$f Q=$qSize"

object Configuration:
  /** Θ_std at its least N = 3f + 1, so Q_size = 2f + 1. */
  def minimalStd(f: Int): Configuration[SoftwareEvidence] =
    val vs = TestValidators.generate(StdTrust.minValidators(f))
    val gen = SoftwareMessageGen(vs, vs.take(f).toSet, Domain.voting)
    Configuration("Θ_std", StdTrust, gen, () => SoftwareRuntime())

  /** Θ_tee at its least N = 2f + 1, so Q_size = f + 1. */
  def minimalTee(f: Int): Configuration[TeeEvidence] =
    val vs = TestValidators.generate(TeeTrust.minValidators(f))
    val gen = TeeMessageGen(vs, vs.take(f).toSet, Domain.voting)
    Configuration("Θ_tee", TeeTrust, gen, () => TeeRuntime())

/** Deterministic oracles: each replica proposes its own block, a block is valid
  * unless its id starts with "invalid", the leader schedule is given.
  */
final class TestApplication(
    self: PublicKey,
    validators: Vector[PublicKey],
    proposer: (Long, Long) => PublicKey
) extends Application:
  def getProposer(h: Long, r: Long): PublicKey = proposer(h, r)
  def isValid(b: Block): Boolean = !b.blockId.startsWith("invalid")
  def getValue(h: Long, r: Long): Block =
    TestApplication.block(s"h$h-r$r-v${validators.indexOf(self)}", h)

object TestApplication:
  def block(id: String, h: Long): Block =
    Block(id, h, ByteVector(id.getBytes("UTF-8")))
  def invalidBlock(h: Long): Block = block(s"invalid-$h", h)

  /** The leader of (h, r) is validator (h + r) mod N. */
  def roundRobin(vs: Vector[PublicKey]): (Long, Long) => PublicKey =
    (h, r) => vs(((h + r) % vs.size).toInt)

/** Which Layer 4 transition produced a state. Ingestion is not one: it shows up
  * between two transitions as a change of storage alone.
  */
enum Kind[E <: Evidence]:
  case Init()
  case Compute()
  case OnTimeout(t: Context)
  case FastForward(qc: TmQC[E])

final case class Transition[E <: Evidence](
    kind: Kind[E],
    before: ReplicaState[E],
    after: ReplicaState[E]
)

/** Records every transition of a protocol, in order, without changing it. */
final class RecordingProtocol[E <: Evidence](inner: ProtocolLogic[E])
    extends ProtocolLogic[E]:
  private type S = ReplicaState[E]
  val log: ArrayBuffer[Transition[E]] = ArrayBuffer.empty

  private def record(k: Kind[E], before: S, after: S): S =
    log += Transition(k, before, after)
    after

  def init(): S = { val s = inner.init(); record(Kind.Init(), s, s) }
  def compute(s: S): S = record(Kind.Compute(), s, inner.compute(s))
  def onTimeout(s: S, t: Context): S =
    record(Kind.OnTimeout(t), s, inner.onTimeout(s, t))
  def fastForward(s: S, qc: TmQC[E]): S =
    record(Kind.FastForward(qc), s, inner.fastForward(s, qc))
  def send(s: S): Set[Outbound[E]] = inner.send(s)

/** One call of F at one replica: the event, the states around it, the effects,
  * the transitions it performed and the timers pending after it.
  */
final case class Call[E <: Evidence](
    event: Event[E],
    before: Option[ReplicaState[E]],
    after: ReplicaState[E],
    effects: Set[Effect[E]],
    transitions: Vector[Transition[E]],
    timersAfter: Set[Context]
)

/** Timing, in ticks. An object arrives after 1 to `delay` ticks, or with
  * probability `slow` after 1 to `slowDelay`, so deliveries overtake one
  * another; with probability `duplicate` a copy follows. A timer of round r
  * fires after timeoutBase + timeoutPerRound · r ticks. At each tick a
  * Byzantine validator acts with probability `byzantine`.
  */
final case class Schedule(
    delay: Int = 10,
    slow: Double = 0.2,
    slowDelay: Int = 60,
    duplicate: Double = 0.1,
    timeoutBase: Int = 15,
    timeoutPerRound: Int = 10,
    byzantine: Double = 0.3
)

/** The correct validators of a configuration running Tendermint under the
  * engine, over a network that delays, reorders and duplicates, and f Byzantine
  * validators that equivocate, sign invalid blocks, forge justifications and
  * relay certificates to arbitrary subsets of replicas.
  */
final class Cluster[E <: Evidence](
    val config: Configuration[E],
    rnd: Random,
    val proposer: (Long, Long) => PublicKey,
    schedule: Schedule
):
  private enum Delivery:
    case Msg(m: TmMessage[E])
    case Cert(qc: TmQC[E])

  val runtime: Runtime[E] = config.newRuntime()
  val members: Vector[PublicKey] = config.validators.members
  val replicas: Vector[PublicKey] = members.filter(config.correct)
  private val byzantine = members.filter(config.byzantine)

  val protocols: Map[PublicKey, RecordingProtocol[E]] =
    replicas.map(v => v -> RecordingProtocol(freshProtocol(v))).toMap
  private val engines = protocols.map((v, p) => v -> StabilizationEngine(p))
  private val admissions = replicas.map(v => v -> runtime.newAdmission()).toMap
  private val current = mutable.Map.empty[PublicKey, ReplicaState[E]]

  val calls: Map[PublicKey, ArrayBuffer[Call[E]]] =
    replicas.map(v => v -> ArrayBuffer.empty[Call[E]]).toMap
  val admitted: Map[PublicKey, ArrayBuffer[TmMessage[E]]] =
    replicas.map(v => v -> ArrayBuffer.empty[TmMessage[E]]).toMap
  val admittedCertificates: mutable.LinkedHashSet[TmQC[E]] =
    mutable.LinkedHashSet.empty

  private var now = 0L
  private val network = ArrayBuffer.empty[(Long, PublicKey, Delivery)]
  private val timers = ArrayBuffer.empty[(Long, PublicKey, Context)]

  def state(v: PublicKey): ReplicaState[E] = current(v)

  def transitionsOf(v: PublicKey): Vector[Transition[E]] =
    calls(v).iterator.flatMap(_.transitions).toVector

  /** Every state of the replica's execution, in order, without repeats. */
  def statesOf(v: PublicKey): Vector[ReplicaState[E]] =
    val all = transitionsOf(v).flatMap(t => Vector(t.before, t.after))
    all.headOption.toVector ++
      all.zip(all.drop(1)).collect { case (a, b) if a != b => b }

  /** An unrecorded instance of the replica's protocol, to probe states. */
  def freshProtocol(v: PublicKey): Tendermint[E] =
    Tendermint(v, config.queries, TestApplication(v, members, proposer))

  def isValid(b: Block): Boolean =
    TestApplication(members.head, members, proposer).isValid(b)

  def start(): this.type =
    replicas.foreach(v => invoke(v, Event.Start()))
    this

  def run(ticks: Int): this.type =
    for _ <- 1 to ticks do step()
    this

  /** One tick: the objects and timers due now, in random order, then maybe a
    * Byzantine action. A timer stays armed until it fires.
    */
  def step(): Unit =
    now += 1
    val due = network.filter(_._1 <= now).toVector
    val dueTimers = timers.filter(_._1 <= now).toVector
    network.filterInPlace(_._1 > now)
    val fire = (t: (Long, PublicKey, Context)) =>
      () =>
        timers -= t
        invoke(t._2, Event.Timeout(t._3))
    val events =
      due.map((_, r, d) => () => deliver(r, d)) ++ dueTimers.map(fire)
    rnd.shuffle(events).foreach(_())
    if byzantine.nonEmpty && rnd.nextDouble() < schedule.byzantine then
      byzantineAct()

  /** Delivers only timers, earliest first: the network is silent. */
  def fireTimers(k: Int): this.type =
    for _ <- 1 to k if timers.nonEmpty do
      val t @ (due, v, ctx) = timers.minBy(_._1)
      timers -= t
      now = math.max(now, due)
      invoke(v, Event.Timeout(ctx))
    this

  private def invoke(v: PublicKey, e: Event[E]): Unit =
    val log = protocols(v).log
    val mark = log.size
    val before = current.get(v)
    val (after, effects) = before.fold(engines(v).start())(engines(v)(_, e))
    current(v) = after
    effects.foreach {
      case Effect.Emit(Outbound.Payload(w, b)) =>
        broadcast(Delivery.Msg(runtime.sign(w, b)))
      case Effect.Emit(Outbound.Certificate(qc)) =>
        broadcast(Delivery.Cert(qc))
      case Effect.ScheduleTimeout(t) =>
        timers += ((now + timeout(t.round), v, t))
    }
    val pending = timers.iterator.collect { case (_, w, t) if w == v => t }
    calls(v) += Call(
      e,
      before,
      after,
      effects,
      log.drop(mark).toVector,
      pending.toSet
    )

  private def timeout(round: Long): Long =
    schedule.timeoutBase + schedule.timeoutPerRound * round

  private def post(to: PublicKey, d: Delivery): Unit =
    def delay(): Long =
      val bound =
        if rnd.nextDouble() < schedule.slow then schedule.slowDelay
        else schedule.delay
      1L + rnd.nextInt(bound)
    network += ((now + delay(), to, d))
    if rnd.nextDouble() < schedule.duplicate then
      network += ((now + delay(), to, d))

  private def broadcast(d: Delivery): Unit = replicas.foreach(post(_, d))

  private def sendToSome(d: Delivery): Unit =
    replicas.foreach(r => if rnd.nextDouble() < 0.6 then post(r, d))

  private def deliver(to: PublicKey, d: Delivery): Unit = d match
    case Delivery.Msg(m) if runtime.validMessage(m) =>
      admissions(to).offer(m).foreach { a =>
        admitted(to) += a
        invoke(to, Event.Message(a))
      }
    case Delivery.Cert(qc) if runtime.validCertificate(qc) =>
      admittedCertificates += qc
      invoke(to, Event.Certificate(qc))
    case _ => ()

  /** A Byzantine validator relays a certificate it can assemble, or signs nil,
    * an invalid block or any block signed at h, maybe justified.
    */
  private def byzantineAct(): Unit =
    val v = byzantine(rnd.nextInt(byzantine.size))
    val maxH = replicas.map(current(_).registers.currentHeight).max
    val maxR = replicas.map(current(_).registers.currentRound).max
    val h = rnd.nextInt(maxH.toInt + 1).toLong
    val r = rnd.nextInt(maxR.toInt + 2).toLong
    def assemble(rd: Long, s: Step) =
      runtime.assemble(h, rd, s, members, config.qSize)
    if rnd.nextInt(4) == 0 then
      assemble(r, Step.PRECOMMIT).foreach(qc => sendToSome(Delivery.Cert(qc)))
    else
      val step = Step.values(rnd.nextInt(Step.values.length))
      val pool = candidates(h)
      val x = pool(rnd.nextInt(pool.size))
      val j =
        if step == Step.PROPOSAL && r > 0 && rnd.nextBoolean() then
          assemble(rnd.nextInt(r.toInt).toLong, Step.PREVOTE)
        else None
      val m = runtime.sign(Slot(h, r, step, v), Body(x, TmMeta(j)))
      sendToSome(Delivery.Msg(m))

  private def candidates(h: Long): Vector[Option[Block]] =
    val signed = runtime.signed.iterator.flatMap(_.body.block)
      .filter(_.height == h).toVector.distinct
    Vector(None, Some(TestApplication.invalidBlock(h))) ++ signed.map(Some(_))

object Cluster:
  /** Starts every correct replica and runs `steps` ticks. */
  def simulate[E <: Evidence](
      config: Configuration[E],
      seed: Long,
      steps: Int,
      schedule: Schedule = Schedule(),
      proposer: Option[(Long, Long) => PublicKey] = None
  ): Cluster[E] =
    val leaders = proposer.getOrElse(
      TestApplication.roundRobin(config.validators.members)
    )
    Cluster(config, new Random(seed), leaders, schedule).start().run(steps)

/** Small steps for hand-written executions. */
object Drive:
  def context[E <: Evidence](s: ReplicaState[E]): Context =
    val r = s.registers
    Context(r.currentHeight, r.currentRound, r.currentStep)

  /** L ← L ⊔ δ_m for each message, in order. */
  def ingest[E <: Evidence](s: ReplicaState[E], ms: TmMessage[E]*) =
    s.copy(storage = ms.foldLeft(s.storage)(_.ingest(_)))

  /** Compute to a fixpoint of the context, bounded so a bug fails. */
  def settle[E <: Evidence](
      p: ProtocolLogic[E],
      s: ReplicaState[E],
      bound: Int = 10000
  ): ReplicaState[E] =
    Iterator.iterate((s, p.compute(s)))((_, b) => (b, p.compute(b)))
      .zipWithIndex
      .map { case ((a, b), i) =>
        assert(i < bound, "Compute did not stop moving the context")
        (a, b)
      }
      .collectFirst { case (a, b) if p.context(a) == p.context(b) => b }
      .get

  /** Ingests the messages one by one, settling after each. */
  def deliver[E <: Evidence](
      p: ProtocolLogic[E],
      s: ReplicaState[E],
      ms: TmMessage[E]*
  ): ReplicaState[E] =
    ms.foldLeft(s)((st, m) => settle(p, ingest(st, m)))

  /** Signs every payload of Send(s). */
  def signSend[E <: Evidence](
      p: ProtocolLogic[E],
      s: ReplicaState[E],
      rt: Runtime[E]
  ): Vector[TmMessage[E]] =
    p.send(s).toVector.collect { case Outbound.Payload(w, b) => rt.sign(w, b) }

  /** A vote at (h, r, step) signed for v. */
  def vote[E <: Evidence](
      rt: Runtime[E],
      v: PublicKey,
      h: Long,
      r: Long,
      step: Step,
      x: Option[Block]
  ): TmMessage[E] =
    rt.sign(Slot(h, r, step, v), Body(x, TmMeta.empty))

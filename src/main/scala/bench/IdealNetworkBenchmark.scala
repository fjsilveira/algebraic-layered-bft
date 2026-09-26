package bench

import blockchain.{StdTrust, TeeTrust, TrustModel, ValidatorsSet}
import crypto.{Evidence, SoftwareEvidence, TeeEvidence}
import domain.*
import layers.*
import scodec.bits.ByteVector
import zio.*

import java.security.{KeyPairGenerator, PublicKey}

/** Signing, outside the formal object: ideal signatures, and under Θ_tee the
 * enclave counter of the signer at the step.
 */
trait Signer[E <: Evidence]:
  def sign(w: Slot, b: Body[TmMeta[E]], counter: Long): E

object Signer:
  private def digest(w: Slot, b: Body[?]) = ByteVector.fromInt((w, b).hashCode)
  val std: Signer[SoftwareEvidence] =
    (w, b, _) => SoftwareEvidence(digest(w, b))
  val tee: Signer[TeeEvidence] = (w, b, c) => TeeEvidence(digest(w, b), c)

/** One configuration of the happy path: trust model, N, network delay per step,
 * batch size (1 is one F call per message) and one fiber per replica.
 */
final case class Setup(
                        trust: String,
                        n: Int,
                        delay: Duration,
                        batch: Int,
                        par: Boolean
                      )

/** What a run of `heights` heights cost. */
final case class Stats(
                        heights: Long,
                        steps: Int,
                        messages: Long,
                        calls: Long,
                        busy: Duration,
                        wall: Duration
                      ):
  def perHeight(d: Duration): Double = d.toNanos / 1e6 / heights

/** N correct replicas on a fault-free network. At every step each replica
 * applies F to the previous batch of wire messages, in chunks of `batch`; what
 * all replicas emit forms the next batch, delivered to every replica after the
 * network delay. Timers are dropped, since no fault needs one.
 */
final class Network[E <: Evidence](
                                    trust: TrustModel[E],
                                    signer: Signer[E],
                                    setup: Setup
                                  ):
  type Msg = TmMessage[E]

  private val keys: Vector[PublicKey] = Vector.fill(setup.n)(
    KeyPairGenerator.getInstance("Ed25519").generateKeyPair().getPublic
  )
  private val queries = QuorumQueries(trust, ValidatorsSet(keys))

  /** Leader (h + r) mod N; each replica proposes a block of its own. */
  private final class App(i: Int) extends Application:
    def getProposer(h: Long, r: Long): PublicKey =
      keys(((h + r) % setup.n).toInt)
    def isValid(b: Block): Boolean = true
    def getValue(h: Long, r: Long): Block =
      Block(s"h$h-r$r-v$i", h, ByteVector.empty)

  /** A replica: its engine, its state and its enclave counters. */
  private final case class Replica(
                                    f: StabilizationEngine[E],
                                    s: ReplicaState[E],
                                    counters: Map[Step, Long]
                                  ):
    def height: Long = s.registers.currentHeight

    /** Signs the payloads of `fx`, advancing the counter of each step. */
    def signed(fx: Iterable[Effect[E]]): (Replica, Vector[Msg]) =
      fx.foldLeft((this, Vector.empty[Msg])) {
        case ((r, out), Effect.Emit(Outbound.Payload(w, b))) =>
          val c = r.counters.getOrElse(w.step, 0L) + 1
          val m = WireMessage(w, b, signer.sign(w, b, c))
          (r.copy(counters = r.counters.updated(w.step, c)), out :+ m)
        case (acc, _) => acc
      }

    /** F over the batch in chunks, and what it emitted, signed. */
    def step(batch: Vector[Msg]): (Replica, Vector[Msg], Int) =
      val events =
        if setup.batch == 1 then batch.map(Event.Message(_))
        else batch.grouped(setup.batch).map(Event.Batch(_)).toVector
      val (r, out) = events.foldLeft((this, Vector.empty[Msg])) {
        case ((r, acc), e) =>
          val (s1, fx) = r.f(r.s, e)
          val (r1, ms) = r.copy(s = s1).signed(fx)
          (r1, acc ++ ms)
      }
      (r, out, events.size)

  private def boot(): (Vector[Replica], Vector[Msg]) =
    val started = keys.indices.toVector.map { i =>
      val f = StabilizationEngine(Tendermint(keys(i), queries, App(i)))
      val (s0, fx) = f.start()
      Replica(f, s0, Map.empty).signed(fx)
    }
    (started.map(_._1), started.flatMap(_._2))

  /** Steps until every replica passes `heights`. */
  def run(heights: Long): Task[Stats] =
    def all(rs: Vector[Replica], b: Vector[Msg]) =
      if setup.par then ZIO.foreachPar(rs)(r => ZIO.succeed(r.step(b)))
      else ZIO.foreach(rs)(r => ZIO.succeed(r.step(b)))
    def loop(rs: Vector[Replica], b: Vector[Msg], st: Stats): Task[Stats] =
      if rs.forall(_.height >= heights) then ZIO.succeed(st)
      else if b.isEmpty then ZIO.fail(RuntimeException("no progress"))
      else
        for
          (t, out) <- all(rs, b).timed
          _ <- ZIO.sleep(setup.delay)
          next = st.copy(
            steps = st.steps + 1,
            messages = st.messages + b.size,
            calls = st.calls + out.map(_._3).sum,
            busy = st.busy + t
          )
          res <- loop(out.map(_._1), out.flatMap(_._2), next)
        yield res
    val (rs, b) = boot()
    val zero = Stats(heights, 0, 0L, 0L, Duration.Zero, Duration.Zero)
    loop(rs, b, zero).timed.map((w, st) => st.copy(wall = w))

object Network:
  def apply(setup: Setup): Network[?] = setup.trust match
    case "std" => new Network(StdTrust, Signer.std, setup)
    case "tee" => new Network(TeeTrust, Signer.tee, setup)

/** The four experiments of the evaluation: scaling with N, Θ_std against Θ_tee
 * at equal f under growing delay, batch size, and Θ_std against Θ_tee at equal
 * f under growing batch size. Every row is the median of `Runs` runs, after a
 * warm-up. What each experiment sweeps is set by the constants below, so a
 * different grid needs no change to the experiments.
 */
object IdealNetworkBenchmark extends ZIOAppDefault:

  // What to run

  /** Heights decided per run, and per warm-up run. */
  val Heights: Long = 1000
  val WarmupHeights: Long = 100

  /** Runs per row; the row reports the median by wall time. */
  val Runs: Int = 3

  /** Scaling: the numbers of replicas, under Θ_std, no delay, batch 1. */
  val Replicas: List[Int] = List(10)

  /** Trust and delay: the resilience f (Θ_std runs N = 3f + 1, Θ_tee runs
   * N = 2f + 1) and the network delays per step, in milliseconds.
   */
  val Faults: List[Int] = List(13)
  val DelaysMs: List[Long] = List(0, 2, 5, 10, 20)

  /** Batching: the batch sizes, at `BatchReplicas` replicas under Θ_std. */
  val Batches: List[Int] = List(1, 5, 10, 20, 40)
  val BatchReplicas: Int = 20

  /** Which experiments to run. */
  val RunScaling: Boolean = false
  val RunTrustAndDelay: Boolean = false
  val RunBatching: Boolean = false
  val RunTrustAndBatching: Boolean = true

  // Experiments

  def median(setup: Setup, heights: Long): Task[Stats] =
    ZIO
      .foreach(1 to Runs)(_ => Network(setup).run(heights))
      .map(xs => xs.sortBy(_.wall).apply(Runs / 2))

  def ms(d: Double): String = f"$d%.2f"

  /** Busy time per height, sequential and one fiber per replica, for every N
   * in `Replicas`.
   */
  val scaling =
    ZIO.foreach(Replicas) { n =>
      for
        seq <- median(Setup("std", n, Duration.Zero, n, false), Heights)
        par <- median(Setup("std", n, Duration.Zero, n, true), Heights)
        (a, b) = (seq.perHeight(seq.busy), par.perHeight(par.busy))
      yield List(
        n.toString,
        (seq.messages / seq.heights).toString,
        ms(a),
        ms(b),
        f"${a / b}%.2f"
      ).mkString(" & ")
    }

  /** Wall time per height at equal f: Θ_std with N = 3f + 1 against Θ_tee with
   * N = 2f + 1, for every f in `Faults` and every delay in `DelaysMs`.
   */
  val trustAndDelay =
    ZIO.foreach(for f <- Faults; d <- DelaysMs yield (f, d)) { (f, d) =>
      val delay = Duration.fromMillis(d)
      val nStd = 3 * f + 1
      val nTee = 2 * f + 1
      for
        std <- median(Setup("std", nStd, delay, nStd, true), Heights)
        tee <- median(Setup("tee", nTee, delay, nTee, true), Heights)
        (a, b) = (std.perHeight(std.wall), tee.perHeight(tee.wall))
      yield List(
        f.toString,
        d.toString,
        (std.messages / std.heights).toString,
        (tee.messages / tee.heights).toString,
        ms(a),
        ms(b),
        ms(a - b),
        f"${a / b}%.2f"
      ).mkString(" & ")
    }

  /** Calls of F and busy time per height for every batch size in `Batches`,
   * sequential, at `BatchReplicas` replicas; the speed-up is against the
   * first size.
   */
  val batching =
    for
      rows <- ZIO.foreach(Batches) { k =>
        median(Setup("std", BatchReplicas, Duration.Zero, k, false), Heights)
          .map(s => (k, s.calls / s.heights, s.perHeight(s.busy)))
      }
      base = rows.headOption.fold(1.0)(_._3)
    yield rows.map((k, calls, busy) =>
      List(k.toString, calls.toString, ms(busy), f"${base / busy}%.2f")
        .mkString(" & ")
    )

  /** Calls of F and busy time per height at equal f: Θ_std with N = 3f + 1
   * against Θ_tee with N = 2f + 1, for every f in `Faults` and every batch size in
   * `Batches`, in parallel without delay.
   */
  val trustAndBatching =
    ZIO.foreach(for f <- Faults; k <- Batches yield (f, k)) { (f, k) =>
      val nStd = 3 * f + 1
      val nTee = 2 * f + 1
      for
        std <- median(Setup("std", nStd, Duration.Zero, k, true), Heights)
        tee <- median(Setup("tee", nTee, Duration.Zero, k, true), Heights)
        (a, b) = (std.perHeight(std.busy), tee.perHeight(tee.busy))
      yield List(
        f.toString,
        k.toString,
        (std.calls / std.heights).toString,
        (tee.calls / tee.heights).toString,
        ms(a),
        ms(b),
        ms(a - b),
        f"${a / b}%.2f"
      ).mkString(" & ")
    }

  def table(header: String, rows: List[String]) =
    Console.printLine(header) *> ZIO.foreachDiscard(rows)(Console.printLine(_))

  def run =
    for
      _ <- median(Setup("std", 10, Duration.Zero, 1, false), WarmupHeights)
      _ <- median(Setup("tee", 10, Duration.Zero, 1, true), WarmupHeights)
      _ <- ZIO.when(RunScaling)(
        scaling.flatMap(table("N & msgs/h & busy seq & busy par & speed-up", _))
      )
      _ <- ZIO.when(RunTrustAndDelay)(
        trustAndDelay.flatMap(
          table("f & d & msgs std & msgs tee & std & tee & gap & ratio", _)
        )
      )
      _ <- ZIO.when(RunBatching)(
        batching.flatMap(table("batch & calls/h & busy & speed-up", _))
      )
      _ <- ZIO.when(RunTrustAndBatching)(
        trustAndBatching.flatMap(
          table("f & k & calls std & calls tee & std busy & tee busy & gap & ratio", _)
        )
      )
    yield ()
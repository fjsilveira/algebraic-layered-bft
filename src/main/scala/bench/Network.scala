package bench

import blockchain.{StdTrust, TeeTrust, TrustModel, ValidatorsSet}
import crypto.Evidence
import domain.*
import layers.*
import scodec.bits.ByteVector
import zio.{Duration, Task, UIO, ZIO}

import java.security.{KeyPairGenerator, PublicKey}

/** N correct replicas on a fault free, synchronous network, run in rounds.
 *
 * In round r every replica applies F to the messages delivered at r, and what
 * all replicas emit while doing so is delivered to every replica at r + 1,
 * together, after one network delay. The delay is therefore paid once per
 * round whatever the batch size: a message emitted early in a round does not
 * arrive before one emitted late, exactly as if the whole round had been sent
 * as a single batch. Batching only changes how many times F runs over the
 * same delivered set (see [[Batch]]). Timers are dropped, since no fault
 * needs one.
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
  private val delay = Duration.fromMillis(setup.delayMs)

  /** Leader (h + r) mod N; each replica proposes a block of its own. */
  private final class App(i: Int) extends Application:
    def getProposer(h: Long, r: Long): PublicKey =
      keys(((h + r) % setup.n).toInt)
    def isValid(b: Block): Boolean = true
    def getValue(h: Long, r: Long): Block =
      Block(s"h$h-r$r-v$i", h, ByteVector.empty)

  /** What one replica did in one round. */
  private final case class Turn(r: Replica, out: Vector[Msg], calls: Int, ns: Long)

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

    /** The events F sees for one delivered round. */
    private def events(inbox: Vector[Msg]): Vector[Event[E]] =
      setup.batch match
        case Batch.PerMessage => inbox.map(Event.Message(_))
        case Batch.Round      => Vector(Event.Batch(inbox))
        case k                => inbox.grouped(k).map(Event.Batch(_)).toVector

    /** F over the delivered round, and what it emitted, signed and timed. */
    def turn(inbox: Vector[Msg]): Turn =
      val t0 = System.nanoTime()
      val es = events(inbox)
      val (r, out) = es.foldLeft((this, Vector.empty[Msg])) {
        case ((r, acc), e) =>
          val (s1, fx) = r.f(r.s, e)
          val (r1, ms) = r.copy(s = s1).signed(fx)
          (r1, acc ++ ms)
      }
      Turn(r, out, es.size, System.nanoTime() - t0)

  private def boot(): (Vector[Replica], Vector[Msg]) =
    val started = keys.indices.toVector.map { i =>
      val f = StabilizationEngine(Tendermint(keys(i), queries, App(i)))
      val (s0, fx) = f.start()
      Replica(f, s0, Map.empty).signed(fx)
    }
    (started.map(_._1), started.flatMap(_._2))

  /** Every replica takes its turn over the same delivered round. */
  private def round(rs: Vector[Replica], inbox: Vector[Msg]) =
    def one(r: Replica) = ZIO.succeed(r.turn(inbox))
    if setup.parallel then ZIO.foreachPar(rs)(one)
    else ZIO.foreach(rs)(one)

  /** The network carries the round: one delay, whatever the batch size. */
  private val deliver: UIO[Unit] =
    if setup.sleep && setup.delayMs > 0 then ZIO.sleep(delay) else ZIO.unit

  /** Rounds until every replica passes `heights`. */
  def run(heights: Long): Task[Stats] =
    def loop(rs: Vector[Replica], inbox: Vector[Msg], st: Stats): Task[Stats] =
      if rs.forall(_.height >= heights) then ZIO.succeed(st)
      else if inbox.isEmpty then ZIO.fail(RuntimeException("no progress"))
      else
        for
          turns <- round(rs, inbox)
          _ <- deliver
          next = st.copy(
            rounds = st.rounds + 1,
            messages = st.messages + inbox.size,
            calls = st.calls + turns.map(_.calls).sum,
            cpu = st.cpu + turns.map(_.ns).sum,
            critical = st.critical + turns.map(_.ns).max,
            network = st.network + delay.toNanos
          )
          res <- loop(turns.map(_.r), turns.flatMap(_.out), next)
        yield res
    val (rs, inbox) = boot()
    val zero = Stats(heights, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
    loop(rs, inbox, zero).timed.map { (wall, st) =>
      val virtual = if setup.sleep then 0L else st.network
      st.copy(latency = wall.toNanos + virtual)
    }

object Network:
  def apply(setup: Setup): Network[?] = setup.trust match
    case Trust.Std => new Network(StdTrust, Signer.std, setup)
    case Trust.Tee => new Network(TeeTrust, Signer.tee, setup)

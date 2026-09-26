package layers

import crypto.Evidence

import scala.annotation.tailrec

/** The engine events: bootstrap, a wire message or a batch of them (storage), a
  * certificate (synchronization) and a timeout stamped with the context it was
  * armed for.
  */
enum Event[E <: Evidence]:
  case Start()
  case Message(m: TmMessage[E])
  case Batch(ms: Vector[TmMessage[E]])
  case Certificate(qc: TmQC[E])
  case Timeout(t: Context)

/** Effects handed to the runtime: something to transmit, or a timer. */
enum Effect[E <: Evidence]:
  case Emit(out: Outbound[E])
  case ScheduleTimeout(t: Context)

/** Layer 5: F(S, e) = (S', E). Each event reaches exactly one Layer 4 entry
  * point; Compute is iterated until the context stops moving, and only then are
  * effects emitted. No protocol rule lives here.
  */
final class StabilizationEngine[E <: Evidence](p: ProtocolLogic[E]):

  private type S = ReplicaState[E]
  private type Out = (S, Set[Effect[E]])

  /** S_0 = Init(), announcing Send(S_0) and arming a timer for it. */
  def start(): Out =
    val s0 = p.init()
    (s0, emitted(s0) + Effect.ScheduleTimeout(p.context(s0)))

  def apply(s: S, e: Event[E]): Out = e match
    case Event.Start() => start()
    case Event.Message(m) =>
      settle(s, p.compute(s.copy(storage = s.storage.ingest(m))))
    case Event.Batch(ms) => // L ⊔ Δ_B, one Compute for the whole batch
      settle(
        s,
        p.compute(s.copy(storage = s.storage.merge(Accumulation.of(ms))))
      )
    case Event.Certificate(qc) => settle(s, p.fastForward(s, qc))
    case Event.Timeout(t) => settle(s, p.onTimeout(s, t))

  /** An event that keeps the context produces no effects. */
  private def settle(s: S, s1: S): Out =
    if p.context(s) == p.context(s1) then (s1, Set.empty)
    else stabilize(s, s1, Set.empty)

  /** Compute until two consecutive states share a context, announcing every
    * context entered; only the fixpoint arms a timer.
    */
  @tailrec
  private def stabilize(s: S, s1: S, acc: Set[Effect[E]]): Out =
    if p.context(s) == p.context(s1) then
      (s1, acc + Effect.ScheduleTimeout(p.context(s1)))
    else stabilize(s1, p.compute(s1), acc ++ emitted(s1))

  private def emitted(s: S): Set[Effect[E]] = p.send(s).map(Effect.Emit(_))

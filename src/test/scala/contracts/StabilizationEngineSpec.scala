package contracts

import crypto.Evidence
import layers.*
import harness.*
import org.scalatest.funsuite.AnyFunSuite
import java.security.PublicKey

/** Contract K_5 of the stabilization engine: the law of Exp_5 and Corollary
  * (Engine Properties).
  *
  * Every call of F at every correct replica of simulations at the least N of
  * each trust model is checked, over an asynchronous network that reorders and
  * duplicates, with f Byzantine validators; the order in which events reach F
  * is the scheduler's, drawn at random.
  */
class StabilizationEngineSpec extends AnyFunSuite:

  private val Runs = 6
  private val Steps = 1500

  private final class Simulation[E <: Evidence](val c: Configuration[E]):
    lazy val clusters: Vector[Cluster[E]] =
      Vector.tabulate(Runs)(seed => Cluster.simulate(c, seed.toLong, Steps))

    def executions: Vector[(Cluster[E], PublicKey)] =
      for cl <- clusters; v <- cl.replicas yield (cl, v)

  private val std =
    Vector(1, 2).map(f => Simulation(Configuration.minimalStd(f)))
  private val tee =
    Vector(1, 2).map(f => Simulation(Configuration.minimalTee(f)))

  private def ctx[E <: Evidence](s: ReplicaState[E]): Context = Drive.context(s)

  private def below[E <: Evidence](
      a: Accumulation[TmMeta[E], E],
      b: Accumulation[TmMeta[E], E]
  ): Boolean =
    a.slots.forall(w => a(w).subsetOf(b(w)))

  // Exp_5

  /** F(S, e) → (S', E): Start emits Send(S_0) and a timer for Context(S_0); a
    * call that keeps the context emits nothing; a call that moves it arms
    * exactly one timer, stamped with the context it returns.
    */
  private def effectsLaw[E <: Evidence](sim: Simulation[E]): Unit =
    var moved = 0
    for (cl, v) <- sim.executions do
      val p = cl.freshProtocol(v)
      for call <- cl.calls(v) do
        val reached = ctx(call.after)
        val timers = call.effects.collect { case Effect.ScheduleTimeout(t) =>
          t
        }
        call.before match
          case None =>
            val announced = p.send(call.after).map(Effect.Emit(_))
            val expected = announced + Effect.ScheduleTimeout(reached)
            assert(call.effects == expected, "Start")
          case Some(before) if ctx(before) == reached =>
            assert(call.effects.isEmpty, s"${call.event} kept $reached")
          case Some(_) =>
            moved += 1
            assert(timers == Set(reached), s"${call.event} armed $timers")
    assert(moved > 0)

  // Thm_5, Corollary (Engine Properties)

  /** Item 1, Lemma (Stabilization): every call returns at a state whose current
    * height holds no decision, and a message call, or a call that moves the
    * context, returns a state whose context Compute does not change.
    */
  private def stabilization[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions do
      val p = cl.freshProtocol(v)
      for call <- cl.calls(v) do
        val r = call.after.registers
        val h = r.currentHeight
        assert(!r.decisions.contains(h), s"${call.event} rests at decided $h")
        val message = call.event match
          case Event.Message(_) => true
          case _ => false
        val moved = call.before.exists(b => ctx(b) != ctx(call.after))
        if message || moved then
          val fixpoint = ctx(p.compute(call.after)) == ctx(call.after)
          assert(fixpoint, s"${call.event} returned before the fixpoint")

  /** Item 2, Lemma (Engine Execution): the states form an execution. It starts
    * at Init; between two transitions only ingestion happens, which keeps the
    * registers and grows storage; every storage is built from the admitted
    * messages, in admission order; and every payload or certificate emitted is
    * the Send of a state of the execution.
    */
  private def engineExecution[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions do
      val p = cl.freshProtocol(v)
      val transitions = cl.transitionsOf(v)
      assert(transitions.head.kind == Kind.Init[E]())
      for (t, next) <- transitions.zip(transitions.tail) do
        if t.after != next.before then
          val (a, b) = (t.after, next.before)
          assert(b.registers == a.registers, "more than ingestion happened")
          assert(below(a.storage, b.storage), "storage shrank")
      val admitted = Accumulation.of(cl.admitted(v))
      assert(cl.state(v).storage == admitted, "storage is not the admitted set")
      val states = cl.statesOf(v)
      states.foreach(s => assert(below(s.storage, admitted)))
      val sends = states.flatMap(p.send).toSet
      for call <- cl.calls(v); case Effect.Emit(o) <- call.effects do
        assert(sends.contains(o), s"emitted $o, the Send of no state")

  /** Item 2, second part: Corollary (Protocol Safety) holds after every call,
    * for every sequence of events: decisions never change at a replica, agree
    * within a round across replicas, and agree across rounds under an
    * admissible configuration.
    */
  private def safetyAfterEveryCall[E <: Evidence](sim: Simulation[E]): Unit =
    var decided = 0
    for cl <- sim.clusters do
      for v <- cl.replicas do
        val calls = cl.calls(v)
        for
          (a, b) <- calls.zip(calls.tail); (h, d) <- a.after.registers.decisions
        do assert(b.after.registers.decisions.get(h).contains(d), s"[$h]")
      val decisions =
        for
          v <- cl.replicas
          call <- cl.calls(v)
          (h, d) <- call.after.registers.decisions
        yield (h, d.qc.slot.round, d.block)
      decided += decisions.distinct.size
      for (h, atH) <- decisions.groupBy(_._1) do
        for (r, atR) <- atH.groupBy(_._2) do
          assert(atR.map(_._3).distinct.size == 1, s"height $h round $r")
        if sim.c.queries.admissible then
          assert(atH.map(_._3).distinct.size == 1, s"height $h")
    assert(decided > 0, "no replica decided")

  /** Item 3, Lemma (Announcement): a call emits the Send of every context it
    * enters.
    */
  private def announcement[E <: Evidence](sim: Simulation[E]): Unit =
    var entered = 0
    for (cl, v) <- sim.executions do
      val p = cl.freshProtocol(v)
      for call <- cl.calls(v) do
        val emitted = call.effects.collect { case Effect.Emit(o) => o }
        val ts = call.transitions
        if call.before.isEmpty then
          assert(p.send(call.after).subsetOf(emitted), "Start")
        else if ts.nonEmpty then
          val visited = ts.head.before +: ts.map(_.after)
          for (prev, s) <- visited.zip(visited.tail) if ctx(prev) != ctx(s) do
            entered += 1
            val msg = s"${call.event} entered ${ctx(s)} silently"
            assert(p.send(s).subsetOf(emitted), msg)
    assert(entered > 0)

  /** Item 4, Lemma (Deadlock Freedom), first part: every context of rest
    * carries an unexpired timer stamped with it, so once timers are delivered
    * no context is a sink.
    */
  private def timerForEveryRest[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions; call <- cl.calls(v) do
      val rest = ctx(call.after)
      assert(call.timersAfter.contains(rest), s"$rest rests without a timer")

  /** Item 4, Lemma (Deadlock Freedom), second part: with a silent network and
    * every timer delivered, no replica decides and each enters unboundedly many
    * rounds of height 0; here, at least 20, every timer call moving its context
    * forward.
    */
  private def unboundedRounds[E <: Evidence](c: Configuration[E]): Unit =
    val cl = Cluster.simulate(c, seed = 0L, steps = 0)
    def behind = cl.replicas.exists(cl.state(_).registers.currentRound < 20)
    var i = 0
    while behind && i < 20000 do
      cl.fireTimers(1)
      i += 1
    for v <- cl.replicas do
      val r = cl.state(v).registers
      val progressed = r.currentHeight == 0 && r.currentRound >= 20
      assert(progressed, s"stuck at ${ctx(cl.state(v))}")
      for call <- cl.calls(v); before <- call.before do
        val msg = s"${call.event} did not move ${ctx(before)}"
        assert(ctx(call.after) > ctx(before), msg)

  /** Remark (Local Batching): delivering the admitted messages in batches of
    * k joins each Δ_B before one Compute, reaches the storage of the admitted
    * set whatever k, and rests at a fixpoint of Compute.
    */
  private def batchedDelivery[E <: Evidence](sim: Simulation[E]): Unit =
    for (cl, v) <- sim.executions; k <- Vector(2, 5, 20) do
      val f = StabilizationEngine(cl.freshProtocol(v))
      val ms = cl.admitted(v).toVector
      val s0 = f.start()._1
      val s = ms.grouped(k).foldLeft(s0)((s, b) => f(s, Event.Batch(b))._1)
      assert(s.storage == Accumulation.of(ms), s"storage differs, k = $k")
      assert(f(s, Event.Batch(Vector.empty))._2.isEmpty, s"moves, k = $k")

  // registration

  private def contract[E <: Evidence](sim: Simulation[E]): Unit =
    val n = sim.c.name
    test(s"K5 Exp (F) [$n]: effects only on a move")(effectsLaw(sim))
    test(s"K5 Thm (1), Lemma (Stabilization) [$n]")(stabilization(sim))
    test(s"K5 Thm (2), Lemma (Engine Execution) [$n]")(engineExecution(sim))
    test(s"K5 Thm (2), Corollary (Protocol Safety) after every call [$n]")(
      safetyAfterEveryCall(sim)
    )
    test(s"K5 Thm (3), Lemma (Announcement) [$n]")(announcement(sim))
    test(s"K5 Thm (4), Lemma (Deadlock Freedom) [$n]: timers at rest")(
      timerForEveryRest(sim)
    )
    test(s"K5 Thm (4), Lemma (Deadlock Freedom) [$n]: unbounded rounds")(
      unboundedRounds(sim.c)
    )
    test(s"Remark (Local Batching) [$n]: batches reach the same fixpoint")(
      batchedDelivery(sim)
    )

  std.foreach(contract)
  tee.foreach(contract)

package contracts

import crypto.{Evidence, SoftwareEvidence}
import domain.*
import layers.*
import harness.*
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import scala.util.Random

/** Contract K_2 of projection: the law of Exp_2 and the items of Corollary
  * (Projection Properties), which make up Thm_2, each with the result it cites.
  */
class ProjectionSpec extends AnyFunSuite:

  private val validators = TestValidators.generate(4)
  private val byzantine = validators.take(2).toSet

  private val software = SoftwareMessageGen(validators, byzantine)
  private val tee = TeeMessageGen(validators, byzantine)

  private def size(rnd: Random): Int = 20 + rnd.nextInt(80)

  /** The final state of one replica of a fresh history. */
  private def replica[E <: Evidence](gen: MessageGen[E], rnd: Random) =
    val history = gen.history(rnd, size(rnd))
    Accumulation.of(gen.replica(rnd, history, rnd.nextInt(20)))

  // Exp_2

  /** Definition (Projection Operator): π(X) ∈ X ∪ {⊥}, so π(∅) = ⊥. */
  private def projectionOperator[E <: Evidence](
      gen: MessageGen[E],
      pi: Projection[E]
  ): Unit =
    Seeds.foreach() { rnd =>
      val l = replica(gen, rnd)
      for w <- l.slots do assert(pi.select(l, w).forall(l(w).contains), s"$w")
      assert(pi.select(l, gen.unpopulatedSlot).isEmpty)
      assert(pi.select(
        Accumulation.empty[String, E],
        gen.unpopulatedSlot
      ).isEmpty)
    }

  // Thm_2, Corollary (Projection Properties)

  /** Item 1, Theorem (Projection Convergence): two states built from the same
    * set of admitted messages read the same value at every slot.
    */
  private def projectionConvergence[E <: Evidence](
      gen: MessageGen[E],
      pi: Projection[E]
  ): Unit =
    Seeds.foreach() { rnd =>
      val history = gen.history(rnd, size(rnd))
      val admitted = gen.admit(rnd, history)
      def state() =
        Accumulation.of(gen.delivery(rnd, admitted, rnd.nextInt(20)))
      val (l1, l2) = (state(), state())
      for w <- gen.slotsOf(history) do
        assert(pi.select(l1, w) == pi.select(l2, w), s"$w")
    }

  /** Item 2, Lemma (Read Stability): if p is the only pair a replica ever
    * admits at ω, every state of that replica reads p at ω from the admission
    * of p on.
    */
  private def readStability[E <: Evidence](
      gen: MessageGen[E],
      pi: Projection[E]
  ): Unit =
    var checked = 0
    Seeds.foreach() { rnd =>
      val history = gen.history(rnd, size(rnd))
      val onlyPair = history
        .groupMap(_.slot)(gen.pair)
        .collect { case (w, ps) if ps.distinct.size == 1 => w -> ps.head }
      val ms = gen.replica(rnd, history, rnd.nextInt(20))
      val states = Replica.statesAlong(ms)
      for (w, p) <- onlyPair do
        val admission = ms.indexWhere(_.slot == w)
        if admission >= 0 then
          checked += 1
          for l <- states.drop(admission + 1) do
            assert(pi.select(l, w).contains(p), s"$w")
    }
    assert(checked > 0, "no slot with a single pair was generated")

  /** Lemma (Unique Minimal Counter), cited by item 3: in every state, the least
    * counter of a populated slot is carried by exactly one pair, the first
    * message of its validator there.
    */
  private def uniqueMinimalCounter(gen: TeeMessageGen): Unit =
    Seeds.foreach() { rnd =>
      val history = gen.history(rnd, size(rnd))
      val first = gen.firstMessages(history)
      val ms = gen.replica(rnd, history, rnd.nextInt(20))
      for l <- Replica.statesAlong(ms); w <- l.slots do
        val least = l(w).map(_._2.counter).min
        val carriers = l(w).filter(_._2.counter == least)
        assert(carriers.size == 1, s"$w: ${carriers.size} least counters")
        assert(carriers.head == gen.pair(first(w)), s"$w")
    }

  /** Item 3, Theorem (Non-Equivocation): under π_tee every non-⊥ read at ω, in
    * any state of any replica, is the first message of its validator there, so
    * any two are equal.
    */
  private def nonEquivocation(gen: TeeMessageGen): Unit =
    Seeds.foreach() { rnd =>
      val history = gen.history(rnd, size(rnd))
      val first = gen.firstMessages(history)
      val replicas = Vector.fill(3)(gen.replica(rnd, history, rnd.nextInt(20)))
      val reads =
        for
          ms <- replicas
          l <- Replica.statesAlong(ms)
          w <- gen.slotsOf(history)
          p <- TeeProjection.select(l, w)
        yield w -> p
      for (w, ps) <- reads.groupMap(_._1)(_._2) do
        assert(ps.distinct.size == 1, s"$w: two non-⊥ reads differ")
        assert(ps.head == gen.pair(first(w)), s"$w: not the first message")
    }

  /** Item 4, Theorem (Equivocation Hiding) under π_std: a slot holding several
    * pairs reads as the empty slot.
    */
  private def equivocationHidingStd(gen: SoftwareMessageGen): Unit =
    var checked = 0
    val empty = Accumulation.empty[String, SoftwareEvidence]
    Seeds.foreach() { rnd =>
      val l = replica(gen, rnd)
      for w <- l.slots if l(w).size >= 2 do
        checked += 1
        assert(StdProjection.select(l, w) == StdProjection.select(empty, w))
    }
    assert(checked > 0, "no equivocated slot was generated")

  /** Item 4, Theorem (Equivocation Hiding) under π_tee: a slot holding several
    * pairs reads as the slot holding only its first message.
    */
  private def equivocationHidingTee(gen: TeeMessageGen): Unit =
    var checked = 0
    Seeds.foreach() { rnd =>
      val history = gen.history(rnd, size(rnd))
      val first = gen.firstMessages(history)
      val l = Accumulation.of(gen.replica(rnd, history, rnd.nextInt(20)))
      for w <- l.slots if l(w).size >= 2 do
        checked += 1
        val read = TeeProjection.select(l, w)
        assert(read == TeeProjection.select(Accumulation.delta(first(w)), w))
        assert(read.contains(gen.pair(first(w))), s"$w")
    }
    assert(checked > 0, "no equivocated slot was generated")

  /** Remark after Definition (Software-Only Projection): π_std contains
    * equivocation locally, so two correct replicas may read different values at
    * the slot of an equivocating validator.
    */
  private def stdEquivocationIsLocal(): Unit =
    val w = Slot(0L, 0L, Step.PREVOTE, validators.head)
    def signed(a: String, sig: Int) =
      WireMessage(w, Body[String](None, a), SoftwareEvidence(ByteVector(sig)))
    val (m1, m2) = (signed("a", 1), signed("b", 2))
    val read1 = StdProjection.select(Accumulation.delta(m1), w)
    val read2 = StdProjection.select(Accumulation.delta(m2), w)
    assert(read1.contains((m1.body, m1.evidence)))
    assert(read2.contains((m2.body, m2.evidence)))
    assert(read1 != read2)

  test("K2 Exp, Definition (Projection Operator), π_std")(
    projectionOperator(software, StdProjection)
  )
  test("K2 Exp, Definition (Projection Operator), π_tee")(
    projectionOperator(tee, TeeProjection)
  )
  test("K2 Thm (1), Theorem (Projection Convergence), π_std")(
    projectionConvergence(software, StdProjection)
  )
  test("K2 Thm (1), Theorem (Projection Convergence), π_tee")(
    projectionConvergence(tee, TeeProjection)
  )
  test("K2 Thm (2), Lemma (Read Stability), π_std")(
    readStability(software, StdProjection)
  )
  test("K2 Thm (2), Lemma (Read Stability), π_tee")(
    readStability(tee, TeeProjection)
  )
  test("K2 Thm (3), Lemma (Unique Minimal Counter)")(
    uniqueMinimalCounter(tee)
  )
  test("K2 Thm (3), Theorem (Non-Equivocation)")(nonEquivocation(tee))
  test("K2 Thm (4), Theorem (Equivocation Hiding), π_std")(
    equivocationHidingStd(software)
  )
  test("K2 Thm (4), Theorem (Equivocation Hiding), π_tee")(
    equivocationHidingTee(tee)
  )
  test("Remark, π_std: equivocation is contained locally")(
    stdEquivocationIsLocal()
  )

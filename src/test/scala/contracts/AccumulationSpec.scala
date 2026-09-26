package contracts

import crypto.Evidence
import layers.*
import harness.*
import org.scalatest.funsuite.AnyFunSuite

/** Contract K_1 of accumulation. Layer 1 is indifferent to the proof carrier,
  * so every check runs under both Σ_proof and ℕ × Σ_proof.
  */
class AccumulationSpec extends AnyFunSuite:

  private val validators = TestValidators.generate(4)
  private val byzantine = validators.take(2).toSet

  private val software = SoftwareMessageGen(validators, byzantine)
  private val tee = TeeMessageGen(validators, byzantine)

  // Exp_1

  /** L_i(ω) is the set of pairs of the messages at ω in the admitted set that
    * built L_i.
    */
  private def readLaw[E <: Evidence](gen: MessageGen[E]): Unit =
    Seeds.foreach() { rnd =>
      val history = gen.history(rnd, 20 + rnd.nextInt(80))
      val admitted = gen.admit(rnd, history)
      val l = Accumulation.of(gen.delivery(rnd, admitted, rnd.nextInt(20)))
      for w <- gen.slotsOf(history) :+ gen.unpopulatedSlot do
        val expected = admitted.filter(_.slot == w).map(gen.pair).toSet
        assert(l(w) == expected, s"$w")
    }

  /** Ingestion is inflationary, L_i ≤ L_i ⊔ δ_m, so an admitted pair is never
    * removed.
    */
  private def inflationary[E <: Evidence](gen: MessageGen[E]): Unit =
    Seeds.foreach() { rnd =>
      val history = gen.history(rnd, 20 + rnd.nextInt(80))
      val ms = gen.replica(rnd, history, rnd.nextInt(20))
      val states = Replica.statesAlong(ms)
      for (before, after) <- states.zip(states.tail); w <- before.slots do
        assert(before(w).subsetOf(after(w)), s"$w")
    }

  // Thm_1

  /** Theorem (Convergence): the state depends on the set of admitted messages
    * alone, not on delivery order, duplication or batching.
    */
  private def convergence[E <: Evidence](gen: MessageGen[E]): Unit =
    Seeds.foreach() { rnd =>
      val history = gen.history(rnd, 20 + rnd.nextInt(80))
      val admitted = gen.admit(rnd, history)
      def schedule() =
        gen.withDuplicates(rnd, rnd.shuffle(admitted), rnd.nextInt(20))
      val (rho, rho2) = (schedule(), schedule())

      val l = Accumulation.of(rho)
      val batched = Replica.ingestBatched(rho2, 1 + rnd.nextInt(8))
      val ofSet = admitted.map(Accumulation.delta)
        .foldLeft(Accumulation.empty[String, E])(_.merge(_))

      assert(Accumulation.of(rho2) == l, "depends on order or duplication")
      assert(batched == l, "depends on batching")
      assert(ofSet == l, "differs from the join of the admitted set")
    }

  test("K1 Exp (read law), Σ_proof")(readLaw(software))
  test("K1 Exp (read law), ℕ × Σ_proof")(readLaw(tee))

  test("K1 Exp (inflationary ingestion), Σ_proof")(inflationary(software))
  test("K1 Exp (inflationary ingestion), ℕ × Σ_proof")(inflationary(tee))

  test("K1 Thm, Theorem (Convergence), Σ_proof")(convergence(software))
  test("K1 Thm, Theorem (Convergence), ℕ × Σ_proof")(convergence(tee))

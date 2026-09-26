package props

import crypto.Evidence
import layers.Accumulation
import harness.AdversarialHarness.*
import harness.Configuration
import org.scalacheck.Prop.forAllNoShrink as forAll
import org.scalacheck.{Prop, Properties}

object L1Spec extends Properties("L1 Accumulation"):

  // [Theorem Convergence: L depends on the admitted set alone]
  def confluence[E <: Evidence](c: Configuration[E]): Prop =
    forAll(genSchedules(c)) { (rho, rho2) =>
      val deltas = rho.map(Accumulation.delta).reduceOption(_.merge(_))
      Accumulation.of(rho) == Accumulation.of(rho2) &&
      deltas.forall(_ == Accumulation.of(rho))
    }

  // [Join-semilattice: idempotent, commutative, associative]
  def semilattice[E <: Evidence](c: Configuration[E]): Prop =
    val g = genAdversarialBatch(c).map(Accumulation.of)
    forAll(g, g, g) { (a, b, x) =>
      a.merge(a) == a && a.merge(b) == b.merge(a) &&
      a.merge(b).merge(x) == a.merge(b.merge(x))
    }

  property("confluence, Σ_proof") = confluence(std)
  property("confluence, ℕ × Σ_proof") = confluence(tee)
  property("semilattice, Σ_proof") = semilattice(std)
  property("semilattice, ℕ × Σ_proof") = semilattice(tee)

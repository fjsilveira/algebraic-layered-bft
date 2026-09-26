package props

import layers.{StdProjection, TeeProjection}
import harness.AdversarialHarness.*
import org.scalacheck.Prop.forAllNoShrink as forAll
import org.scalacheck.Properties

object L2Spec extends Properties("L2 Projection"):

  // [Theorem Equivocation Hiding, π_std: an equivocated slot reads as ⊥]
  property("equivocation hiding, Θ_std") =
    forAll(genEquivocation(std)) { (w, _, l) =>
      StdProjection.select(l, w).isEmpty
    }

  // [Theorem Non-Equivocation, π_tee: the least counter, the first signed]
  property("non-equivocation, Θ_tee") =
    forAll(genEquivocation(tee)) { (w, first, l) =>
      TeeProjection.select(l, w).contains((first.body, first.evidence))
    }

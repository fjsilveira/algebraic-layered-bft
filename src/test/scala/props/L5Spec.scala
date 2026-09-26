package props

import harness.AdversarialHarness.*
import org.scalacheck.Prop.forAllNoShrink as forAll
import org.scalacheck.{Properties, Test}

object L5Spec extends Properties("L5 Stabilization Engine"):

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(20)

  // [Theorem Termination: under the bounded delays of the simulation every
  // correct replica decides]
  property("termination, Θ_std") = forAll(genExecution(std))(_.everyoneDecided)
  property("termination, Θ_tee") = forAll(genExecution(tee))(_.everyoneDecided)

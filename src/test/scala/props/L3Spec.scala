package props

import crypto.Evidence
import harness.AdversarialHarness.*
import harness.Configuration
import org.scalacheck.Prop.forAllNoShrink as forAll
import org.scalacheck.{Prop, Properties, Test}

object L3Spec extends Properties("L3 Quorum Queries"):

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(20)

  // [Theorems Quorum Validity (2), Corollary Quorum Queries (2a): two
  // grounded certificates at one (h, r, s) carry the same value, by honesty
  // under Θ_std and by the enclave under Θ_tee]
  def quorumValidity[E <: Evidence](c: Configuration[E]): Prop =
    forAll(genExecution(c)) { x =>
      x.certificates.groupBy(_.slot).values
        .forall(_.map(_.value).distinct.size == 1)
    }

  // [Lemma Query Convergence: the same admitted set gives the same answer
  // to every query]
  def convergence[E <: Evidence](c: Configuration[E]): Prop =
    forAll(genVotingPair(c)) { (l1, l2) =>
      val q = c.queries
      coordinates(c).forall { w =>
        val (h, r, s) = (w.height, w.round, w.step)
        q.evalQC(l1, h, r, s) == q.evalQC(l2, h, r, s) &&
        q.highestQC(l1, h, r, s) == q.highestQC(l2, h, r, s) &&
        q.syncRound(l1, h, r) == q.syncRound(l2, h, r)
      }
    }

  property("quorum validity, Θ_std") = quorumValidity(std)
  property("quorum validity, Θ_tee") = quorumValidity(tee)
  property("query convergence, Θ_std") = convergence(std)
  property("query convergence, Θ_tee") = convergence(tee)

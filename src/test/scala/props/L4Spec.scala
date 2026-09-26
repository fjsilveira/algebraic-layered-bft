package props

import crypto.Evidence
import domain.Step
import layers.Context
import harness.AdversarialHarness.*
import harness.Configuration
import org.scalacheck.Prop.forAllNoShrink as forAll
import org.scalacheck.{Prop, Properties, Test}

object L4Spec extends Properties("L4 Protocol Logic"):

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(20)

  // [Lemma Lock Invariant: every correct replica that enters
  // (h, r, PRECOMMIT) with a block locks on the certificate evalQC returns
  // at (h, r, PREVOTE) for that block]
  def lockInvariant[E <: Evidence](c: Configuration[E]): Prop =
    forAll(genExecution(c)) { x =>
      val broken =
        for
          run <- x.runs
          t <- run.transitions
          Context(h, rd, step) = run.p.context(t.after)
          r = t.after.registers
          if run.p.context(t.before) != run.p.context(t.after)
          if step == Step.PRECOMMIT && r.vote.isDefined
          lock = x.queries.evalQC(t.before.storage, h, rd, Step.PREVOTE)
          if r.lockedQC != lock || !lock.exists(_.value == r.vote)
        yield t
      broken.isEmpty
    }

  // Θ_std: every correct replica, every execution

  property("lock invariant, Θ_std") = lockInvariant(std)

  // [Theorem Lock Safety: a later prevote certificate at h carries the
  // block of any precommit certificate at h]
  property("lock safety, Θ_std") = forAll(genExecution(std)) { x =>
    val qcs = x.certificates.filter(_.value.isDefined)
    val conflicts =
      for
        pc <- qcs if pc.slot.step == Step.PRECOMMIT
        pv <- qcs if pv.slot.step == Step.PREVOTE
        if pv.slot.height == pc.slot.height && pv.slot.round > pc.slot.round
        if pv.value != pc.value
      yield (pc, pv)
    conflicts.isEmpty
  }

  // [Theorem Agreement: one block decided per height]
  property("agreement, Θ_std") = forAll(genExecution(std)) { x =>
    x.decisions.groupBy(_._1).values.forall(_.map(_._3).distinct.size == 1)
  }

  // [Theorem Integrity: a recorded decision never changes]
  property("integrity, Θ_std") = forAll(genExecution(std)) { x =>
    x.runs.forall { run =>
      run.states.zip(run.states.tail).forall { (a, b) =>
        val next = b.registers.decisions
        a.registers.decisions.forall((h, d) => next.get(h).contains(d))
      }
    }
  }

  // Θ_tee: correct replicas keep their locks, Byzantine ones need not

  property("lock invariant, Θ_tee") = lockInvariant(tee)

  // [Remark Lock Safety under Θ_tee: the certificates of two rounds share
  // only Byzantine validators; they precommit x at r and then prevote y at
  // r' > r, ignoring the lock they took at r, and two correct replicas
  // decide different blocks at one height]
  property("lock violation, Θ_tee") = forAll(genTeeSplit) { split =>
    val (a, b, pv) = (split.first, split.second, split.prevote)
    val locked = a.qc.proof.validators.toSet
    val reneged = pv.proof.validators.filter(locked)
    a.block != b.block && pv.value.contains(b.block) &&
    pv.slot.round > a.qc.slot.round &&
    reneged.nonEmpty && reneged.forall(split.byzantine)
  }

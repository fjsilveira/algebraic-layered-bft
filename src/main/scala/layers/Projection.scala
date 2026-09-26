package layers

import crypto.{Evidence, SoftwareEvidence, TeeEvidence}
import domain.{Body, Slot}

/** Layer 2: π(L(ω)) ∈ L(ω) ∪ {⊥}. */
trait Projection[E <: Evidence]:
  def select[A](l: Accumulation[A, E], w: Slot): Option[(Body[A], E)]

/** π_std: a singleton reads as itself, an equivocated slot as ⊥. */
object StdProjection extends Projection[SoftwareEvidence]:
  def select[A](l: Accumulation[A, SoftwareEvidence], w: Slot) =
    val xs = l(w)
    if xs.sizeIs == 1 then xs.headOption else None

/** π_tee: the pair of least counter, the first the enclave attested. */
object TeeProjection extends Projection[TeeEvidence]:
  def select[A](l: Accumulation[A, TeeEvidence], w: Slot) =
    l(w).minByOption(_._2.counter)

package bench

import crypto.{Evidence, SoftwareEvidence, TeeEvidence}
import domain.*
import layers.*
import scodec.bits.ByteVector

/** Signing, outside the formal object: ideal signatures, and under Θ_tee the
 * enclave counter of the signer at the step.
 */
trait Signer[E <: Evidence]:
  def sign(w: Slot, b: Body[TmMeta[E]], counter: Long): E

object Signer:
  private def digest(w: Slot, b: Body[?]) = ByteVector.fromInt((w, b).hashCode)
  val std: Signer[SoftwareEvidence] =
    (w, b, _) => SoftwareEvidence(digest(w, b))
  val tee: Signer[TeeEvidence] = (w, b, c) => TeeEvidence(digest(w, b), c)

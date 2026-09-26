package crypto

import scodec.bits.ByteVector

/** Individual proof. Compared by content, so a re-delivered message is equal to
  * the first delivery and the carrier of Layer 1 stays idempotent.
  */
sealed trait Evidence

/** Σ_proof under Θ_std. */
final case class SoftwareEvidence(signature: ByteVector) extends Evidence

/** ℕ × Σ_proof under Θ_tee (Definition TEE-Backed Proof). */
final case class TeeEvidence(signature: ByteVector, counter: Long)
    extends Evidence

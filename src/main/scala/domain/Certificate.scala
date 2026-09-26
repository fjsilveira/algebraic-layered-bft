package domain

import crypto.Evidence
import java.security.PublicKey

/** p_qc ∈ Π_qc. (V, A, S) is one sequence of triples, so positions align by
  * construction, and `from` rejects repeated validators, so k is honest.
  */
final case class QuorumEvidence[A, E <: Evidence] private (
    entries: Vector[(PublicKey, A, E)]
):
  def size: Int = entries.size
  def validators: Vector[PublicKey] = entries.map(_._1)

object QuorumEvidence:
  def from[A, E <: Evidence](
      es: Vector[(PublicKey, A, E)]
  ): Option[QuorumEvidence[A, E]] =
    Option.when(es.view.map(_._1).toSet.size == es.size)(QuorumEvidence(es))

/** qc = ⟨ω_qc, x, p_qc⟩ ∈ QC = Ω_⊥ × X_⊥ × Π_qc. */
final case class QuorumCertificate[A, E <: Evidence](
    slot: CollapsedSlot,
    value: Option[Block],
    proof: QuorumEvidence[A, E]
)

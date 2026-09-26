package domain

import crypto.Evidence
import scodec.bits.ByteVector

/** x ∈ X, compared by content. */
final case class Block(blockId: String, height: Long, payload: ByteVector)

/** b = (x, a) ∈ B = X_⊥ × A, with None for nil. */
final case class Body[A](block: Option[Block], metadata: A)

/** m = (ω, b, σ) ∈ M = Ω × B × Σ. */
final case class WireMessage[A, E <: Evidence](
    slot: Slot,
    body: Body[A],
    evidence: E
)

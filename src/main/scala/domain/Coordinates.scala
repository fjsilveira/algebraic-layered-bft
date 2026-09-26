package domain

import java.security.PublicKey

/** S, ordered PROPOSAL ≺ PREVOTE ≺ PRECOMMIT by ordinal. */
enum Step:
  case PROPOSAL, PREVOTE, PRECOMMIT

/** ω = (h, r, s, v) ∈ Ω: the contribution of one validator. */
final case class Slot(
    height: Long,
    round: Long,
    step: Step,
    validator: PublicKey
)

/** (h, r, s, ⊥) ∈ Ω_⊥: an outcome attributable to no single validator. */
final case class CollapsedSlot(height: Long, round: Long, step: Step):
  def at(v: PublicKey): Slot = Slot(height, round, step, v)

package layers

import crypto.Evidence
import domain.{Body, Slot, WireMessage}

/** Layer 1: L ∈ Ω → P(B × Σ), a join-semilattice under pointwise union. L is
  * stored curried by height, so a query at h touches only the slots of h
  * (header-based filtering) and the cost of a step does not grow with the
  * length of the chain.
  */
final case class Accumulation[A, E <: Evidence](
    byHeight: Map[Long, Map[Slot, Set[(Body[A], E)]]]
):
  /** L(ω). */
  def apply(w: Slot): Set[(Body[A], E)] = at(w.height).getOrElse(w, Set.empty)

  /** The populated slots of height h. */
  def at(h: Long): Map[Slot, Set[(Body[A], E)]] =
    byHeight.getOrElse(h, Map.empty)

  /** Every populated slot. */
  def slots: Iterable[Slot] = byHeight.view.values.flatMap(_.keys)

  /** L ⊔ L', pointwise at both levels. */
  def merge(that: Accumulation[A, E]): Accumulation[A, E] =
    Accumulation(Accumulation.join(byHeight, that.byHeight)(
      Accumulation.join(_, _)(_ ++ _)
    ))

  /** L ⊔ δ_m without building δ_m; a duplicate returns L itself. */
  def ingest(m: WireMessage[A, E]): Accumulation[A, E] =
    val (w, h, p) = (m.slot, m.slot.height, (m.body, m.evidence))
    val xs = apply(w)
    if xs(p) then this
    else Accumulation(byHeight.updated(h, at(h).updated(w, xs + p)))

object Accumulation:
  def empty[A, E <: Evidence]: Accumulation[A, E] = Accumulation(Map.empty)

  /** δ_m, the least state holding m. */
  def delta[A, E <: Evidence](m: WireMessage[A, E]): Accumulation[A, E] =
    empty.ingest(m)

  /** ⊥ ⊔ δ_m1 ⊔ ... ⊔ δ_mk, the state of a delivery sequence. */
  def of[A, E <: Evidence](
      ms: IterableOnce[WireMessage[A, E]]
  ): Accumulation[A, E] =
    ms.iterator.foldLeft(empty[A, E])((l, m) => l.ingest(m))

  /** Pointwise join of two maps, folding the smaller into the larger. */
  private def join[K, V](a: Map[K, V], b: Map[K, V])(f: (V, V) => V) =
    val (big, small) = if a.size >= b.size then (a, b) else (b, a)
    small.foldLeft(big) { case (acc, (k, v)) =>
      acc.updatedWith(k)(o => Some(o.fold(v)(f(_, v))))
    }

package layers

import blockchain.{TrustModel, ValidatorsSet}
import crypto.Evidence
import domain.*

/** Layer 3: the quorum queries under Θ = (π, N, Q_size). Storage is read only
  * through readSlot, that is π(L(ω)); the keys of L only enumerate the rounds
  * worth asking about. N, f, Q_size and π_qc stay private (C_3).
  */
final class QuorumQueries[E <: Evidence](
    trust: TrustModel[E],
    validators: ValidatorsSet
):
  private val n = validators.size
  private val f = trust.maxFaulty(n)
  private val q = trust.quorumSize(n)

  type L[A] = Accumulation[A, E]
  type QC[A] = QuorumCertificate[A, E]

  /** Two quorums share a correct validator: 2 Q_size − N ≥ f + 1. */
  val admissible: Boolean = 2 * q - n > f

  /** Definition (Slot Read): readSlot(L, ω) = π(L(ω)). */
  def readSlot[A](l: L[A], w: Slot): Option[(Body[A], E)] =
    trust.projection.select(l, w)

  /** Definition (Round Synchronization): the highest r' > r at h with f + 1
    * witnesses, in one pass over the slots of height h.
    */
  def syncRound[A](l: L[A], h: Long, r: Long): Option[Long] =
    l.at(h).keysIterator
      .filter(w => w.round > r && witness(l, w))
      .toVector
      .groupMapReduce(_.round)(w => Set(w.validator))(_ ++ _)
      .collect { case (r2, vs) if vs.size > f => r2 }
      .maxOption

  /** evalQC: the certificate at (h, r, s, ⊥), or ⊥. */
  def evalQC[A](l: L[A], h: Long, r: Long, s: Step): Option[QC[A]] =
    val c = CollapsedSlot(h, r, s)
    quorum(l, c).map((x, p) => QuorumCertificate(c, x, p))

  /** highestQC: evalQC at the highest round ≤ r where it is not ⊥. */
  def highestQC[A](l: L[A], h: Long, r: Long, s: Step): Option[QC[A]] =
    l.at(h).keysIterator
      .collect { case w if w.round <= r => w.round }
      .toVector
      .distinct
      .sortBy(-_)
      .iterator
      .flatMap(evalQC(l, h, _, s))
      .nextOption()

  /** hasNilQC: a quorum on the nil candidate. */
  def hasNilQC[A](l: L[A], h: Long, r: Long, s: Step): Boolean =
    quorum(l, CollapsedSlot(h, r, s)).exists(_._1.isEmpty)

  /** Definition (Certificate Verification): Q_size members of V, distinct by
    * construction of Π_qc.
    */
  def verifyQC[A](qc: QC[A]): Boolean =
    qc.proof.size >= q &&
      qc.proof.entries.forall(e => validators.contains(e._1))

  /** The quorum search I and π_qc: reads grouped by candidate in one pass,
    * listed in the order of V. The witness is unique, since two disjoint
    * evidence sets of size N − f would need N ≤ 2f.
    */
  private def quorum[A](l: L[A], c: CollapsedSlot) =
    validators.members
      .flatMap(v =>
        readSlot(l, c.at(v)).map((b, e) => (b.block, (v, b.metadata, e)))
      )
      .groupMap(_._1)(_._2)
      .collectFirst { case (x, es) if es.size >= q => (x, es) }
      .flatMap((x, es) => QuorumEvidence.from(es).map(p => (x, p)))

  /** v ∈ W(L, h, r'): a member of V with a non-⊥ read at ω. */
  private def witness[A](l: L[A], w: Slot): Boolean =
    validators.contains(w.validator) && readSlot(l, w).isDefined

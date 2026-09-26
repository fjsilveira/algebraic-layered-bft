package harness

import crypto.{Evidence, SoftwareEvidence, TeeEvidence}
import domain.*
import layers.{TmMessage, TmMeta, TmQC}
import scodec.bits.ByteVector
import java.security.PublicKey
import scala.collection.mutable

/** The runtime of the system model: it signs (ideal signatures, and under Θ_tee
  * enclave counters) and admits messages and certificates. One instance serves
  * a whole simulation, so it knows every message ever signed, which is what "a
  * signature verifies" means for ideal signatures.
  */
trait Runtime[E <: Evidence]:
  def sign(slot: Slot, body: Body[TmMeta[E]]): TmMessage[E]
  def signed: collection.Set[TmMessage[E]]
  def certifiable: collection.Set[TmMessage[E]]
  def newAdmission(): Admission[E]

  /** Definition (Valid Certificate): each entry was signed at the slot, and
    * under Θ_tee is the first message of its validator there.
    */
  final def validCertificate(qc: TmQC[E]): Boolean =
    qc.proof.entries.forall { (v, a, e) =>
      certifiable(WireMessage(qc.slot.at(v), Body(qc.value, a), e))
    }

  /** A message is admitted if it verifies, with its embedded certificate. */
  final def validMessage(m: TmMessage[E]): Boolean =
    signed(m) && m.body.metadata.justification.forall(validCertificate)

  /** A certificate at (h, r, s) from certifiable messages, if one exists. */
  final def assemble(
      h: Long,
      r: Long,
      s: Step,
      vs: Vector[PublicKey],
      q: Int
  ): Option[TmQC[E]] =
    val c = CollapsedSlot(h, r, s)
    val at = certifiable.toVector.filter { m =>
      m.slot.height == h && m.slot.round == r && m.slot.step == s
    }
    at.groupBy(_.body.block).collectFirst {
      case (x, ms) if ms.map(_.slot.validator).distinct.size >= q =>
        val es = vs.flatMap { v =>
          ms.find(_.slot.validator == v)
            .map(m => (v, m.body.metadata, m.evidence))
        }
        QuorumEvidence.from(es).map(QuorumCertificate(c, x, _))
    }.flatten

/** Admission of wire messages at one replica. */
trait Admission[E <: Evidence]:
  /** The messages admitted now, in admission order. */
  def offer(m: TmMessage[E]): Vector[TmMessage[E]]

private def signature(slot: Slot, body: Body[?]): ByteVector =
  ByteVector.fromInt((slot, body).hashCode)

/** Θ_std: admitted on the signature alone. */
final class SoftwareRuntime extends Runtime[SoftwareEvidence]:
  private val log = mutable.LinkedHashSet.empty[TmMessage[SoftwareEvidence]]

  def sign(slot: Slot, body: Body[TmMeta[SoftwareEvidence]]) =
    val m = WireMessage(slot, body, SoftwareEvidence(signature(slot, body)))
    log += m
    m

  def signed: collection.Set[TmMessage[SoftwareEvidence]] = log
  def certifiable: collection.Set[TmMessage[SoftwareEvidence]] = log
  def newAdmission(): Admission[SoftwareEvidence] = m => Vector(m)

/** Θ_tee: one counter per (validator, step), from 1, each position attested
  * once (Definition Counter Context).
  */
final class TeeRuntime extends Runtime[TeeEvidence]:
  private type M = TmMessage[TeeEvidence]
  private val log = mutable.LinkedHashSet.empty[M]
  private val firsts = mutable.LinkedHashSet.empty[M]
  private val first = mutable.Map.empty[Slot, M]
  private val counters = mutable.Map.empty[(PublicKey, Step), Long]

  def sign(slot: Slot, body: Body[TmMeta[TeeEvidence]]): M =
    val ctx = (slot.validator, slot.step)
    val c = counters.getOrElse(ctx, 0L) + 1
    counters(ctx) = c
    val m = WireMessage(slot, body, TeeEvidence(signature(slot, body), c))
    log += m
    if !first.contains(slot) then
      first(slot) = m
      firsts += m
    m

  def signed: collection.Set[M] = log
  def certifiable: collection.Set[M] = firsts
  def newAdmission(): Admission[TeeEvidence] = TeeAdmission()

/** Under Θ_tee a message of context (v, s) with counter c is admitted only when
  * c = last(v, s) + 1: one ahead waits, one behind is discarded.
  */
final class TeeAdmission extends Admission[TeeEvidence]:
  private type M = TmMessage[TeeEvidence]
  private val last = mutable.Map.empty[(PublicKey, Step), Long]
  private val waiting =
    mutable.Map.empty[(PublicKey, Step), mutable.Map[Long, M]]

  def offer(m: M): Vector[M] =
    val ctx = (m.slot.validator, m.slot.step)
    def next = last.getOrElse(ctx, 0L) + 1
    if m.evidence.counter < next then Vector.empty
    else
      val queue = waiting.getOrElseUpdate(ctx, mutable.Map.empty)
      queue(m.evidence.counter) = m
      val out = Vector.newBuilder[M]
      while queue.contains(next) do
        out += queue.remove(next).get
        last(ctx) = next
      out.result()

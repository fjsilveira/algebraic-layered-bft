package layers

import crypto.Evidence
import domain.*
import java.security.PublicKey

/** Tendermint metadata A: for a proposal, the prevote certificate of an earlier
  * round that justifies it (Encapsulated Flow); empty for votes.
  */
final case class TmMeta[E <: Evidence](
    justification: Option[QuorumCertificate[TmMeta[E], E]]
)

object TmMeta:
  def empty[E <: Evidence]: TmMeta[E] = TmMeta(None)

type TmQC[E <: Evidence] = QuorumCertificate[TmMeta[E], E]
type TmMessage[E <: Evidence] = WireMessage[TmMeta[E], E]

/** ⟨currentHeight, currentRound, currentStep⟩, ordered lexicographically. */
final case class Context(height: Long, round: Long, step: Step)
    extends Ordered[Context]:
  def compare(o: Context): Int =
    if height != o.height then height.compare(o.height)
    else if round != o.round then round.compare(o.round)
    else step.ordinal.compare(o.step.ordinal)

/** An entry of decisions: the decided block and its precommit certificate. */
final case class Decision[E <: Evidence](block: Block, qc: TmQC[E])

/** The protocol registers R. */
final case class Registers[E <: Evidence](
    decisions: Map[Long, Decision[E]],
    currentHeight: Long,
    currentRound: Long,
    currentStep: Step,
    lockedQC: Option[TmQC[E]],
    validQC: Option[TmQC[E]],
    proposal: Option[Block],
    vote: Option[Block]
):
  /** The committed ledger: decided blocks of heights 0, 1, ... up to the first
    * undecided height.
    */
  def ledger: Vector[Block] =
    Iterator.iterate(0L)(_ + 1).map(decisions.get).takeWhile(_.isDefined)
      .flatten.map(_.block).toVector

/** S = ⟨L, R⟩. */
final case class ReplicaState[E <: Evidence](
    storage: Accumulation[TmMeta[E], E],
    registers: Registers[E]
):
  def set(r: Registers[E]): ReplicaState[E] = copy(registers = r)

/** What Send emits, unsigned: signing belongs to the runtime. */
enum Outbound[E <: Evidence]:
  case Payload(slot: Slot, body: Body[TmMeta[E]])
  case Certificate(qc: TmQC[E])

/** The oracles GetProposer, IsValid and GetValue of the system model. */
trait Application:
  def getProposer(h: Long, r: Long): PublicKey
  def isValid(block: Block): Boolean
  def getValue(h: Long, r: Long): Block

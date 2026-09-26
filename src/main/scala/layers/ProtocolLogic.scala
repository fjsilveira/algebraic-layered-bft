package layers

import crypto.Evidence

/** Exp_4, what Layer 4 offers Layer 5: four total transitions over ⟨L, R⟩, the
  * output function Send and the accessor Context. Only Init writes L.
  */
trait ProtocolLogic[E <: Evidence]:
  def init(): ReplicaState[E]
  def compute(s: ReplicaState[E]): ReplicaState[E]
  def onTimeout(s: ReplicaState[E], t: Context): ReplicaState[E]
  def fastForward(s: ReplicaState[E], qc: TmQC[E]): ReplicaState[E]
  def send(s: ReplicaState[E]): Set[Outbound[E]]

  final def context(s: ReplicaState[E]): Context =
    val r = s.registers
    Context(r.currentHeight, r.currentRound, r.currentStep)

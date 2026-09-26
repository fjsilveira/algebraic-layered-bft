package blockchain

import crypto.{Evidence, SoftwareEvidence, TeeEvidence}
import layers.{Projection, StdProjection, TeeProjection}

/** Θ = (π, N, Q_size), with Q_size = N − f. */
sealed trait TrustModel[E <: Evidence]:
  def projection: Projection[E]
  def maxFaulty(n: Int): Int
  def minValidators(f: Int): Int
  final def quorumSize(n: Int): Int = n - maxFaulty(n)

/** Θ_std = (π_std, N ≥ 3f + 1, N − f). */
object StdTrust extends TrustModel[SoftwareEvidence]:
  val projection: Projection[SoftwareEvidence] = StdProjection
  def maxFaulty(n: Int): Int = (n - 1) / 3
  def minValidators(f: Int): Int = 3 * f + 1

/** Θ_tee = (π_tee, N ≥ 2f + 1, N − f). */
object TeeTrust extends TrustModel[TeeEvidence]:
  val projection: Projection[TeeEvidence] = TeeProjection
  def maxFaulty(n: Int): Int = (n - 1) / 2
  def minValidators(f: Int): Int = 2 * f + 1

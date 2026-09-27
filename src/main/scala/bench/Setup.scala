package bench

import blockchain.{StdTrust, TeeTrust, TrustModel}

/** The two trust models under comparison. At equal resilience f, Θ_std runs
 * N = 3f + 1 replicas and Θ_tee runs N = 2f + 1.
 */
enum Trust(val label: String, val model: TrustModel[?]):
  case Std extends Trust("std", StdTrust)
  case Tee extends Trust("tee", TeeTrust)

  def replicas(f: Int): Int = model.minValidators(f)
  def quorum(f: Int): Int = model.quorumSize(replicas(f))

/** How the messages delivered in one round are handed to F. `Batch.PerMessage`
 * is one F call per message, `Batch.Round` is one F call for the whole round,
 * and any k > 1 is one F call per chunk of k messages.
 */
object Batch:
  val PerMessage: Int = 1
  val Round: Int = 0

  def label(k: Int): String =
    if k == Round then "round" else k.toString

/** One configuration of the happy path.
 *
 * @param trust
 *   the trust model
 * @param n
 *   the number of replicas
 * @param batch
 *   the chunk size handed to F, see [[Batch]]
 * @param delayMs
 *   the one way network delay, paid once per round
 * @param parallel
 *   one fiber per replica, as if every replica ran on its own machine
 * @param sleep
 *   pay the delay with a real sleep; otherwise it is added to a virtual clock
 */
final case class Setup(
    trust: Trust,
    n: Int,
    batch: Int,
    delayMs: Long,
    parallel: Boolean,
    sleep: Boolean
)

/** What one run of `heights` heights cost. Times are in nanoseconds.
 *
 * @param rounds
 *   delivery rounds, each paying the network delay once
 * @param messages
 *   messages broadcast (each is delivered to all N replicas)
 * @param calls
 *   calls of F, summed over replicas
 * @param cpu
 *   time inside F, summed over replicas: the total work
 * @param critical
 *   per round, the time of the slowest replica, summed over rounds: the
 *   compute on the critical path when every replica has its own machine
 * @param network
 *   rounds times the delay
 * @param latency
 *   the time to decide all heights, network included
 */
final case class Stats(
    heights: Long,
    rounds: Long,
    messages: Long,
    calls: Long,
    cpu: Long,
    critical: Long,
    network: Long,
    latency: Long
):
  /** A count per height. */
  def per(x: Long): Double = x.toDouble / heights

  /** A time per height, in milliseconds. */
  def msPer(ns: Long): Double = ns / 1e6 / heights

  /** Heights decided per second. */
  def throughput: Double = heights / (latency / 1e9)

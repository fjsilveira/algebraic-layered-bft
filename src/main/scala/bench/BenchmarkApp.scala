package bench

import bench.experiments.Experiment
import zio.*

/** How a test runs.
 *
 * @param runs
 *   runs per row; the row reports the median by latency
 * @param warmupHeights
 *   heights per warm-up run before anything is measured, 0 for none
 * @param parallel
 *   one fiber per replica, as if each replica had its own machine
 * @param sleepDelay
 *   pay the network delay with a real sleep, or on a virtual clock
 * @param freshJvm
 *   every run in a child virtual machine of its own, started cold
 * @param latex
 *   also print the table as LaTeX rows
 */
final case class Settings(
    runs: Int = 3,
    warmupHeights: Long = 100,
    parallel: Boolean = true,
    sleepDelay: Boolean = true,
    freshJvm: Boolean = false,
    latex: Boolean = false
)

/** One test of the evaluation as a program of its own. Under sbt every
 * `runMain` forks a new virtual machine, so no test inherits the JIT state
 * left by another.
 */
abstract class BenchmarkApp extends ZIOAppDefault:
  def settings: Settings
  def experiment: Experiment

  /** The resilience of N replicas under Θ_std, f = (N − 1) / 3. Θ_std then
   * runs 3f + 1 replicas and Θ_tee runs 2f + 1 at the same f.
   */
  protected def faultsFor(n: Int): Int = Trust.Std.model.maxFaulty(n)

  def run =
    val c = settings
    val runner = Runner(c.runs, c.parallel, c.sleepDelay, c.freshJvm)
    for
      _ <- ZIO.when(c.warmupHeights > 0 && !c.freshJvm)(
        runner.warmup(c.warmupHeights)
      )
      t <- experiment.run(runner)
      _ <- Console.printLine("\n" + t.render)
      _ <- ZIO.when(c.latex)(Console.printLine("\n" + t.latex))
    yield ()

package bench

import zio.{Task, UIO, ZIO}

/** Runs configurations: every row is the median, by latency, of `runs` runs.
 * The execution mode (one fiber per replica, real or virtual delay, and
 * whether each run gets a virtual machine of its own) is shared by every row
 * of a test.
 */
final class Runner(
    runs: Int,
    parallel: Boolean,
    sleep: Boolean,
    freshJvm: Boolean
):

  def setup(trust: Trust, n: Int, batch: Int, delayMs: Long): Setup =
    Setup(trust, n, batch, delayMs, parallel, sleep)

  /** One run, here or in a child virtual machine that starts cold. */
  private def once(s: Setup, heights: Long): Task[Stats] =
    if freshJvm then Fork.run(s, heights) else Network(s).run(heights)

  def median(s: Setup, heights: Long): Task[Stats] =
    ZIO
      .foreach((1 to runs).toList)(_ => once(s, heights))
      .map(xs => xs.sortBy(_.latency).apply(runs / 2))

  /** Median at resilience f under `trust`, with a progress line. */
  def measure(
      trust: Trust,
      f: Int,
      batch: Int,
      delayMs: Long,
      heights: Long
  ): Task[Stats] =
    val n = trust.replicas(f)
    progress(
      s"${trust.label} f=$f N=$n batch=${Batch.label(batch)} " +
        s"delay=${delayMs}ms heights=$heights"
    ) *> median(setup(trust, n, batch, delayMs), heights)

  /** Lets the JIT compile F before anything is measured. */
  def warmup(heights: Long): Task[Unit] =
    progress(s"warm-up, $heights heights") *>
      ZIO.foreachDiscard(Trust.values.toList)(t =>
        median(setup(t, 10, Batch.PerMessage, 0), heights) *>
          median(setup(t, 10, Batch.Round, 0), heights)
      )

  def mode: String =
    val fibers = if parallel then "one fiber per replica" else "sequential"
    val clock = if sleep then "real delay" else "virtual delay"
    val jvm = if freshJvm then ", fresh JVM per run, no warm-up" else ""
    s"median of $runs, $fibers, $clock$jvm"

  private def progress(s: String): UIO[Unit] =
    ZIO.succeed(System.err.println(s"  .. $s"))

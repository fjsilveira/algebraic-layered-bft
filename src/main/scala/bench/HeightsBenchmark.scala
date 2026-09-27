package bench

import bench.experiments.HeightScaling

/** Test 4. Height scaling: 20 replicas, 100 to 6400 heights, full batching,
 * no delay. Every run starts in a fresh virtual machine with no warm-up, so
 * the table shows how the cost per height falls as the JIT compiles F.
 */
object HeightsBenchmark extends BenchmarkApp:
  val settings = Settings(warmupHeights = 0, freshJvm = true)

  val experiment = HeightScaling(
    f = faultsFor(20),
    heights = List(100, 200, 400, 800, 1600, 3200, 6400),
    batch = Batch.Round,
    delayMs = 0
  )

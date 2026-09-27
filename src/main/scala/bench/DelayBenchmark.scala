package bench

import bench.experiments.BatchingUnderDelay

/** Test 2. Θ_std against Θ_tee under network delay: 20 replicas, delays of 0,
 * 2, 5, 10 and 20 ms, batch 10, 100 heights.
 */
object DelayBenchmark extends BenchmarkApp:
  val settings = Settings()

  val experiment = BatchingUnderDelay(
    f = faultsFor(20),
    heights = 100,
    delaysMs = List(0, 2, 5, 10, 20),
    batches = List(10)
  )

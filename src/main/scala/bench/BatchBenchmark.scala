package bench

import bench.experiments.Batching

/** Test 3. Θ_std against Θ_tee under batching: 40 replicas, batches of 1, 5,
 * 10, 20 and 40, no delay, 1000 heights.
 */
object BatchBenchmark extends BenchmarkApp:
  val settings = Settings()

  val experiment = Batching(
    f = faultsFor(40),
    heights = 1000,
    batches = List(1, 5, 10, 20, 40)
  )

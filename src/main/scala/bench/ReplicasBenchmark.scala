package bench

import bench.experiments.ReplicaScaling

/** Test 1. Replica scaling: 10, 20, 40 and 80 replicas under Θ_std against
 * Θ_tee at the same f, 1000 heights, full batching, no delay.
 */
object ReplicasBenchmark extends BenchmarkApp:
  val settings = Settings()

  val experiment = ReplicaScaling(
    faults = List(10, 20, 40, 80).map(faultsFor),
    heights = 1000,
    batch = Batch.Round,
    delayMs = 0
  )

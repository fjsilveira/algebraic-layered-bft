package bench.experiments

import bench.*
import bench.Table.*
import zio.*

/** Test 1. Scaling with the number of replicas at equal resilience: for every f,
 * Θ_std with N = 3f + 1 against Θ_tee with N = 2f + 1.
 */
final case class ReplicaScaling(
    faults: List[Int],
    heights: Long,
    batch: Int,
    delayMs: Long
) extends Experiment:

  def run(runner: Runner): Task[Table] =
    ZIO
      .foreach(faults) { f =>
        for
          std <- runner.measure(Trust.Std, f, batch, delayMs, heights)
          tee <- runner.measure(Trust.Tee, f, batch, delayMs, heights)
        yield Vector(
          row(f, Trust.Std, std, std.latency.toDouble / tee.latency),
          row(f, Trust.Tee, tee, 1.0)
        )
      }
      .map(sections =>
        Table(
          "Test 1. Replica scaling (std against tee at equal f)",
          s"heights $heights, batch ${Batch.label(batch)}, " +
            s"delay $delayMs ms, ${runner.mode}",
          Vector(
            "f",
            "trust",
            "N",
            "quorum",
            "msgs/h",
            "F calls/h",
            "cpu ms/h",
            "crit ms/h",
            "latency ms/h",
            "heights/s",
            "vs tee"
          ),
          sections.toVector,
          Vector(
            "cpu: time in F summed over replicas; crit: slowest replica per " +
              "round; vs tee: latency over the tee latency at the same f."
          )
        )
      )

  private def row(f: Int, t: Trust, s: Stats, vs: Double) =
    Vector(
      f.toString,
      t.label,
      t.replicas(f).toString,
      t.quorum(f).toString,
      num(s.per(s.messages)),
      num(s.per(s.calls)),
      ms(s.msPer(s.cpu)),
      ms(s.msPer(s.critical)),
      ms(s.msPer(s.latency)),
      num(s.throughput),
      ratio(vs)
    )

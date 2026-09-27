package bench.experiments

import bench.*
import bench.Table.*
import zio.*

/** Test 4. Growing the number of heights decided: the cost per height should
 * stay flat, since a replica only keeps the state of its current height. Both
 * trust models at the same f.
 */
final case class HeightScaling(
    f: Int,
    heights: List[Long],
    batch: Int,
    delayMs: Long
) extends Experiment:

  def run(runner: Runner): Task[Table] =
    ZIO
      .foreach(heights) { h =>
        ZIO.foreach(Trust.values.toList) { t =>
          runner.measure(t, f, batch, delayMs, h).map { s =>
            Vector(
              h.toString,
              t.label,
              t.replicas(f).toString,
              fixed(s.latency / 1e9, 3),
              ms(s.msPer(s.cpu)),
              ms(s.msPer(s.critical)),
              ms(s.msPer(s.latency)),
              num(s.throughput)
            )
          }
        }
      }
      .map(sections =>
        Table(
          "Test 4. Height scaling (std against tee)",
          s"f $f, batch ${Batch.label(batch)}, delay $delayMs ms, " +
            runner.mode,
          Vector(
            "heights",
            "trust",
            "N",
            "total s",
            "cpu ms/h",
            "crit ms/h",
            "latency ms/h",
            "heights/s"
          ),
          sections.map(_.toVector).toVector
        )
      )

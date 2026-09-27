package bench.experiments

import bench.*
import bench.Table.*
import zio.*

/** Test 3. Batch size, without network delay: how the number of F calls and
 * the compute per height fall as each call absorbs more messages, for both
 * trust models at the same f.
 */
final case class Batching(
    f: Int,
    heights: Long,
    batches: List[Int]
) extends Experiment:

  def run(runner: Runner): Task[Table] =
    ZIO
      .foreach(Trust.values.toList) { t =>
        for
          rows <- ZIO.foreach(batches)(k =>
            runner.measure(t, f, k, 0, heights).map(k -> _)
          )
          base = rows.headOption.fold(1.0)(_._2.critical.toDouble)
        yield rows.toVector.map((k, s) =>
          Vector(
            t.label,
            t.replicas(f).toString,
            Batch.label(k),
            num(s.per(s.calls)),
            ms(s.msPer(s.cpu)),
            ms(s.msPer(s.critical)),
            ms(s.msPer(s.latency)),
            num(s.throughput),
            ratio(base / s.critical)
          )
        )
      }
      .map(sections =>
        Table(
          "Test 3. Std against tee under batching",
          s"f $f, heights $heights, no delay, ${runner.mode}",
          Vector(
            "trust",
            "N",
            "batch",
            "F calls/h",
            "cpu ms/h",
            "crit ms/h",
            "latency ms/h",
            "heights/s",
            "speed-up"
          ),
          sections.toVector,
          Vector(
            s"speed-up: critical path compute of batch " +
              s"${batches.headOption.fold("?")(Batch.label)} over this row."
          )
        )
      )

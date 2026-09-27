package bench.experiments

import bench.*
import bench.Table.*
import zio.*

/** Test 2. Batching under network delay: for every delay, one F call per
 * message against one F call for the whole round, for both trust models at the
 * same f. Every message of a round arrives together after one delay, so the
 * network time per height is the same in every row of a given delay and trust;
 * only the number of F calls, and so the compute, changes.
 */
final case class BatchingUnderDelay(
    f: Int,
    heights: Long,
    delaysMs: List[Long],
    batches: List[Int]
) extends Experiment:

  def run(runner: Runner): Task[Table] =
    val cells = for d <- delaysMs; t <- Trust.values.toList yield (d, t)
    ZIO
      .foreach(cells) { (d, t) =>
        for
          rows <- ZIO.foreach(batches)(k =>
            runner.measure(t, f, k, d, heights).map(k -> _)
          )
          base = rows.headOption.fold(1.0)(_._2.latency.toDouble)
        yield rows.toVector.map((k, s) =>
          Vector(
            d.toString,
            t.label,
            t.replicas(f).toString,
            Batch.label(k),
            num(s.per(s.rounds)),
            num(s.per(s.calls)),
            ms(s.msPer(s.network)),
            ms(s.msPer(s.critical)),
            ms(s.msPer(s.latency)),
            ratio(base / s.latency)
          )
        )
      }
      .map(sections =>
        Table(
          "Test 2. Std against tee under network delay",
          s"f $f, heights $heights, ${runner.mode}",
          Vector(
            "delay ms",
            "trust",
            "N",
            "batch",
            "rounds/h",
            "F calls/h",
            "net ms/h",
            "crit ms/h",
            "latency ms/h",
            "speed-up"
          ),
          sections.toVector,
          Vector(
            "net: rounds times the delay, paid once per round whatever the " +
              "batch; speed-up: latency of the first batch over this row."
          )
        )
      )

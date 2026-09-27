package bench.experiments

import bench.{Runner, Table}
import zio.*

/** One test of the evaluation, run for both trust models. */
trait Experiment:
  def run(runner: Runner): Task[Table]

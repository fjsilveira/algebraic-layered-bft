package bench

import zio.{Task, ZIO, ZIOAppDefault, Console}

import java.nio.file.Paths
import scala.io.Source
import scala.jdk.CollectionConverters.*

/** One run in a child virtual machine, started with the classpath of this one
 * and no warm-up, so that it measures a JVM as cold as a fresh deployment.
 */
object Fork:
  private val Tag = "STATS"

  def encode(st: Stats): String =
    (Tag +: st.productIterator.map(_.toString).toList).mkString(" ")

  def decode(line: String): Option[Stats] =
    line.trim.split(" ").toList match
      case Tag :: xs if xs.size == 8 =>
        xs.map(_.toLong) match
          case List(h, r, m, c, cpu, crit, net, lat) =>
            Some(Stats(h, r, m, c, cpu, crit, net, lat))
          case _ => None
      case _ => None

  def args(s: Setup, heights: Long): List[String] =
    List(
      s.trust.label,
      s.n.toString,
      s.batch.toString,
      s.delayMs.toString,
      s.parallel.toString,
      s.sleep.toString,
      heights.toString
    )

  def parse(xs: List[String]): Option[(Setup, Long)] = xs match
    case List(t, n, b, d, p, sl, h) =>
      Trust.values
        .find(_.label == t)
        .map(tr =>
          (Setup(tr, n.toInt, b.toInt, d.toLong, p.toBoolean, sl.toBoolean),
            h.toLong)
        )
    case _ => None

  def run(s: Setup, heights: Long): Task[Stats] =
    ZIO.attemptBlocking {
      val java = Paths.get(System.getProperty("java.home"), "bin", "java")
      val cmd = List(
        java.toString,
        "-cp",
        System.getProperty("java.class.path"),
        "bench.ForkedRun"
      ) ++ args(s, heights)
      val p = ProcessBuilder(cmd.asJava)
        .redirectError(ProcessBuilder.Redirect.INHERIT)
        .start()
      val out = Source.fromInputStream(p.getInputStream).getLines().toList
      val code = p.waitFor()
      out.flatMap(decode).headOption.getOrElse(
        throw RuntimeException(s"child run failed (exit $code): $cmd")
      )
    }

/** The entry point of a child run: one run of the given setup, printed as a
 * single line for the parent.
 */
object ForkedRun extends ZIOAppDefault:
  def run =
    for
      args <- getArgs
      (s, heights) <- ZIO
        .fromOption(Fork.parse(args.toList))
        .orElseFail(RuntimeException(s"bad arguments: ${args.mkString(" ")}"))
      st <- Network(s).run(heights)
      _ <- Console.printLine(Fork.encode(st))
    yield ()

package bench

/** A result table, printed as an ASCII box on the console and, on demand, as LaTeX
 * rows. Rows come in sections, separated by a rule; numeric cells are right
 * aligned.
 */
final case class Table(
    title: String,
    params: String,
    header: Vector[String],
    sections: Vector[Vector[Vector[String]]],
    notes: Vector[String] = Vector.empty
):
  private val rows = sections.flatten

  private val widths: Vector[Int] =
    header.indices.toVector.map(c =>
      (header(c) +: rows.map(_(c))).map(_.length).max
    )

  private def numeric(s: String) =
    s.nonEmpty && s.forall(ch => ch.isDigit || ".-x".contains(ch))

  private def line(c: Char) =
    widths.map(w => c.toString * (w + 2)).mkString("+", "+", "+")

  private def cells(xs: Vector[String], head: Boolean) =
    xs.zip(widths)
      .map { (s, w) =>
        val pad = " " * (w - s.length)
        if !head && numeric(s) then s" $pad$s " else s" $s$pad "
      }
      .mkString("|", "|", "|")

  def render: String =
    val body = sections
      .map(_.map(cells(_, head = false)).mkString("\n"))
      .mkString("\n" + line('-') + "\n")
    Vector(
      title,
      params,
      line('-'),
      cells(header, head = true),
      line('='),
      body,
      line('-')
    ).mkString("\n") + notes.map("\n  " + _).mkString

  def latex: String =
    (s"% $title: ${header.mkString(" & ")}" +:
      rows.map(_.mkString("", " & ", " \\\\"))).mkString("\n")

object Table:
  /** Fixed point with a dot whatever the default locale of the machine. */
  def fixed(x: Double, digits: Int): String =
    String.format(java.util.Locale.ROOT, s"%.${digits}f", Double.box(x))

  def ms(x: Double): String = fixed(x, 3)
  def num(x: Double): String = fixed(x, 1)
  def ratio(x: Double): String = fixed(x, 2) + "x"

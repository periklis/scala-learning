package experiments

package object operators extends App {
  val f1 = new Fraction(1, 2)
  val f2 = new Fraction(2, 4)
  assert(f1 + f2 == new Fraction(1, 1), f"Asssertion ${f1} + ${f2} result shoulbe 1/1")
  assert(f1 - f2 == new Fraction(0, 2))
  assert(f1 * f2 == new Fraction(1, 4))
  assert(f1 / f2 == new Fraction(1, 1))

  val f3 = new Fraction(1, 2)
  val f4 = new Fraction(3, 7)
  assert(f3 + f4 == new Fraction(13, 14), f"Asssertion ${f3} + ${f4} result shoulbe 13/14")
  assert(f3 - f4 == new Fraction(1, 14))
  assert(f3 * f4 == new Fraction(3, 14))
  assert(f3 / f4 == new Fraction(7, 6))

  // val table = new HtmlTable(List(List("Java", "Scala"), List("Gosling", "Odersky")))
  var table = HtmlTable() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
  assert(
    "<table><tr><td>Java</td><td>Scala</td></tr><tr><td>Gosling</td><td>Odersky</td></tr><tr><td>JVM</td><td>JVM, .NET</td></tr></table>"
      == table.toString
  )
}

package operators {

  class Fraction(n: Int, d: Int) {
    import scala.math._

    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d);
    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d);

    override def toString = s"$num/$den"

    override def equals(other: Any) = {
      this.num == other.asInstanceOf[Fraction].num &&
      this.den == other.asInstanceOf[Fraction].den
    }

    def sign(a: Int) =
      if (a > 0) 1 else if (a < 0) -1 else 0

    def gcd(a: Int, b: Int): Int =
      if (b == 0) abs(a)
      else gcd(b, a % b)

    def +(that: Fraction) = this.mapOp(that, _ + _, (a,_) => a)
    def -(that: Fraction) = this.mapOp(that, _ - _, (a,_) => a)
    def *(that: Fraction) = this.mapOp(that, _ * _, _ * _)
    def /(that: Fraction) = this.mapOp(that.flip, _ * _ , _ * _)

    private def mapOp(
      that: Fraction,
      fN: (Int, Int) => Int,
      fD: (Int, Int) => Int): Fraction = {

      val num = fN(this.num * that.den, that.num * this.den)
      val den = this.den * that.den

      new Fraction(num , fD(den, den))
    }

    private def flip(): Fraction = new Fraction(this.den, this.num)
  }

  object HtmlTable {
    def apply() = new HtmlTable()
  }

  class HtmlTable(private val table: List[List[String]] = List(List())) {

    override def toString() =
      this.table.
      foldRight("") {
        (l, b) => b + (l.foldRight("") {
          (s, b) =>  b + s.toColumnStr
        }).toRowStr
      }.toTableStr

    def |(str: String): HtmlTable =
      new HtmlTable((str :: this.table.head) :: this.table.tail)

    def ||(str: String): HtmlTable =
      new HtmlTable(List(str) :: this.table)

    implicit class TableString(str: String) {
      def toRowStr = f"<tr>${str}</tr>"
      def toColumnStr = f"<td>${str}</td>"
      def toTableStr = f"<table>${str}</table>"
    }

  }
}

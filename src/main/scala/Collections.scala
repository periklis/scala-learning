package experiments

package object collections extends App {

  def charIndices(s: String) = {
    s.zipWithIndex.foldLeft(Map[Char, Set[Int]]()) {
      (m, e: (Char, Int)) => {
        m + (e._1 -> (m.getOrElse(e._1, Set()) + e._2))
      }
    }
  }
  println(charIndices("Mississipi"))

  val prices = List(1.0, 2.0, 3.0)
  val quantities = List(2, 3, 4)

  println((prices zip quantities) map {Function.tupled(_ * _)})

  val one = Array(1, 2, 3, 4, 5, 6)

  for(e <- one.grouped(3)) println(e.mkString(", "))
}

package collections {


}

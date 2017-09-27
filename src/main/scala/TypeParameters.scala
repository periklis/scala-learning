package experiments

package object typeparameters extends App {

  val p = new Pair(42, "test")
  val s = p.swap
  assert(p.second == s.first)
  assert(p.first == s.second)

  val mp = new MutPair(1, 2)
  val pm = mp.swap
  assert(pm.first == 2)
  assert(pm.second == 1)

  def genswap[T, S](p: Pair[T,S]): Pair[S,T] =
    new Pair(p.second, p.first)

  val p1 = new Pair('a', 0)
  val p2 = genswap(p1)
  assert(p1.second == p2.first)
  assert(p1.first == p2.second)

  val pp = new BookPair(new Person("Joe"), new Person("John"))
  val st = new Student("Alice", 1234)
  val npp = pp.replaceFirst(st)

  assert(st == npp.first)

  def middle[T](as: Iterable[T]): T =
    if (as.size <= 2) as.iterator.next()
    else as.slice(as.size/2, as.size/2 + 1).iterator.next()

  assert('w' == middle("w"))
  assert('w' == middle("wo"))
  assert('r' == middle("World"))
  assert(1 == middle(List(1)))
  assert(1 == middle(List(1,2)))
  assert(2 == middle(List(1,2,3)))

  val mp2 = new MutPair2(3, 2)
  mp2.swap
  assert(mp2.first == 2)
  assert(mp2.second == 3)

  val mpst = new MutPair2(2, "Hello")
  // Won't compile mpst.swap because cannot prove that Int =:= String.
}

package typeparameters {

  class Pair[T, S](val first: T, val second: S) {
    def swap = new Pair(second, first)
  }

  class MutPair[T](var first: T, var second: T)  {
    def swap: MutPair[T] = {
      val tmp = first
      first = second
      second = tmp
      this
    }
  }

  class MutPair2[T,S](var first: T, var second: S)  {
    def swap(implicit ev: T =:= S, ev1: S =:= T) = {
      val tmp = first
      first = second
      second = tmp
      this
    }
  }

  class Person(val name: String)
  class Student(name: String, val id: Int) extends Person(name)

  // T is here invariant
  class BookPair[T](val first: T, val second: T) {

    // T is here contravariant
    def replaceFirst(newFirst: T) =
      /* T is here covariant, thus we
       * can exchange BookPair[Person] >: BookPair[Student]
       * and Student is already a Person
       */
      new BookPair[T](newFirst, second)
  }
}

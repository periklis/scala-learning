package experiments

import scala.beans.BeanProperty

object Main {
  def main(args: Array[String]) = {}

  def javaLoop(n: Int): Unit = {
    for (from <- 0 to n; i = n - from) {
      println(i)
    }
  }

  def countdown(n: Int) = javaLoop(n)

  def productOfLetters(s: String) = {
    var x: Long = 1
    for (c <- s) x *= c.toLong
    x
  }

  def productOfLettersWoLoop(s: String) = {
    s.foldLeft(1L)((z, c) => z * c.toLong)
  }

  def productOfLettersRecursive(s: String): Long = {
    if (s.length == 0) 1 else s(0).toLong * productOfLettersRecursive(s.substring(1, s.length()))
  }

  assert(Main.productOfLetters("Hello") == 9415087488L)
  assert(Main.productOfLettersWoLoop("Hello") == 9415087488L)
  assert(Main.productOfLettersRecursive("Hello") == 9415087488L)

  def xPowerN(x: Long, n: Int): Float = {
    if (n < 0) {
      1 / xPowerN(x, -n)

    } else if (n > 0 && n % 2 == 0) {
      xPowerN(x, n / 2) * xPowerN(x, n / 2)

    } else if (n > 0 && n % 2 != 0) {
      x * xPowerN(x, n - 1)

    } else {
      1L
    }
  }

  assert(Main.xPowerN(2, 0) == 1)
  assert(Main.xPowerN(2, -2) == 0.25)
  assert(Main.xPowerN(2, 2) == 4)
  assert(Main.xPowerN(2, 3) == 8)

  class Counter(private var value: Int) {
    def increment() = {
      if (value + 1 != Int.MinValue) {
        value += 1
      }
    }

    def current = value
  }

  def getCounter(): Int = {
    val counter = new Counter(Int.MaxValue)

    counter.increment
    counter.increment
    counter.increment
    counter.current
  }

  assert(getCounter() == Int.MaxValue)

  /*
   * Chapter 5
   */

  class Time(private var _hours: Int, private var _mins: Int) {

    private var minutesPastMidNight = _hours * 60 + _mins

    def hours = minutesPastMidNight / 60

    def mins = minutesPastMidNight % 60

    def before(other: Time) =
      minutesPastMidNight < other.minutesPastMidNight
  }

  assert(new Time(10, 20).before(new Time(11, 23)))
  assert(new Time(10, 20).before(new Time(10, 23)))
  assert(!new Time(10, 20).before(new Time(10, 20)))

  class Student(@BeanProperty var name: String, @BeanProperty var id: Long) {}

  class Person(var age: Int) {
    if (age < 0) age = 0
  }

  assert(new Person(20).age == 20)
  assert(new Person(-20).age == 0)
  assert(new Person(0).age == 0)

  class PersonWithName(val name: String) {
    var firstName = name.takeWhile(_ != ' ')
    var lastName = name.dropWhile(_ != ' ').trim
  }

  val person = new PersonWithName("Joe Doe")
  assert(person.firstName == "Joe")
  assert(person.lastName == "Doe")

  class Car(val manufacturer: String, val model: String) {

    private var _modelYear = -1
    var licence = ""

    def this(_manufacturer: String, model: String, _modelYear: Int) = {
      this(_manufacturer, model)
      this._modelYear = _modelYear
    }

    def this(_manufacturer: String, model: String, licence: String) = {
      this(_manufacturer, model)
      this.licence = licence
    }

    def this(manufacturer: String, model: String, _modelYear: Int, licence: String) = {
      this(manufacturer, model)
      this._modelYear = _modelYear
      this.licence = licence
    }

    def modelYear = _modelYear
  }

  val car1 = new Car("BMW", "i3")
  assert(car1.manufacturer == "BMW")
  assert(car1.model == "i3")
  assert(car1.modelYear == -1)
  assert(car1.licence == "")

  class Employee(val name: String = "John Q. Public", var salary: Double = 0.0) {}

  /*
   * Chapter 6
   */

  object Conversions {
    def inchesToCentimeters(value: Double) = value * 2.54

    def gallonsToLiters(value: Double) = value * 3.785

    def milesToKilometers(value: Double) = value * 1.609
  }

  assert(Conversions.inchesToCentimeters(1) == 2.54)
  assert(Conversions.gallonsToLiters(1) == 3.785)
  assert(Conversions.milesToKilometers(1) == 1.609)

  abstract class UnitConversion {
    def convert(value: Double): Double
  }

  object InchesToKilometers extends UnitConversion {
    def convert(value: Double) = value * 2.54
  }

  assert(InchesToKilometers.convert(1) == 2.54)

  object GallonsToLiters extends UnitConversion {
    def convert(value: Double) = value * 3.785
  }

  assert(GallonsToLiters.convert(1) == 3.785)

  object MilesToKilometers extends UnitConversion {
    def convert(value: Double) = value * 1.609
  }

  assert(MilesToKilometers.convert(1) == 1.609)

  object Origin extends java.awt.Point {}

  // Seems very odd to call an instance method on a companion object
  Origin.move(2, 2)

  class Point(val x: Int, val y: Int) {}

  object Point {
    def apply(x: Int, y: Int) = new Point(x, y)
  }

  assert(Point(3,4).x == 3 && Point(4,4).y == 4)


  object PlayingCards extends Enumeration {
    type PlayingCards = Value;
    val spades = Value("\u2660")
    val diamonds = Value("\u2666")
    val hearts = Value("\u2665")
    val clubs = Value("\u2663")
  }

  for (p <- PlayingCards.values) println(s"${p.id}: ${p}")

  def isRedPlayingCard(card: PlayingCards.PlayingCards) = {
    if (card == PlayingCards.hearts || card == PlayingCards.diamonds) true else false
  }

  assert(isRedPlayingCard(PlayingCards.hearts) == true)
  assert(isRedPlayingCard(PlayingCards.diamonds) == true)
}

/*
 * Chapter 6
 */
object Reverser extends App {
  println(f"${args.reverse.mkString(" ")}")
}

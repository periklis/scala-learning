package experiments

package object inheritance extends App {
  val checkings = new CheckingAccount(10)
  assert(checkings.deposit(10) == 19)
  assert(checkings.withdraw(10) == 8)

  val savings = new SavingsAccount(10);
  assert(savings.withdraw(1) == 9)
  assert(savings.deposit(1) == 10)
  assert(savings.withdraw(1) == 9)
  assert(savings.withdraw(1) == 7)
  savings.earnMonthlyInterest()
  assert(savings.withdraw(7 + 0.25*7) == 0)

  val bundle = Bundle(Item(1.0, "Item 1"), Item(2.0, "Item 2"), Item(3.0, "Item 3"))
  assert(bundle.price == 6.0)
  assert(bundle.description == "Item 1,Item 2,Item 3")
  bundle.add(Item(4.0, "Item 4"))
  assert(bundle.price == 10.0)
  assert(bundle.description == "Item 1,Item 2,Item 3,Item 4")

  val pointValue = PointValue(2,2)
  assert(pointValue.x == 2)
  assert(pointValue.y == 2)
  assert(pointValue.packed == 8589934594L)
  assert(pointValue.toString == "2x2")
}

package inheritance{
  class BankAccount(initialBalance: Double) {
    protected var balance = initialBalance

    def deposit(value: Double) = {
      balance += value
      balance
    }

    def withdraw(value: Double) = {
      balance -= value
      balance
    }
  }

  class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    private val charge: Double = 1.0

    override def deposit(value: Double) = super.deposit(value - charge)
    override def withdraw(value: Double) = super.withdraw(value + charge)
  }

  class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    private var charge = 1.0
    private val interestRate = 0.25
    private var transactionCount = 0
    private val maxTransactions = 3

    override def deposit(value: Double) =
      if (transactionCount != maxTransactions) {
        transactionCount += 1
        super.deposit(value)
      } else {
        super.deposit(value - charge)
      }

    override def withdraw(value: Double) =
      if (transactionCount != maxTransactions) {
        transactionCount += 1
        super.withdraw(value)
      } else {
        super.withdraw(value + charge)
      }

    def earnMonthlyInterest() = {
      transactionCount = 0
      balance += balance * interestRate
    }
  }


  abstract class Item {
    def price: Double
    def description: String
  }

  object Item {
    def apply(price: Double, desc: String) = new SimpleItem(price, desc)
  }

  class SimpleItem(val price: Double, val description: String) extends Item {}

  object Bundle {
    def apply(args: Item*) = new Bundle(args.toArray)
  }

  class Bundle(items: Array[Item]) extends Item {
    private var _items = items

    def price = _items.foldLeft(0.0)(_ + _.price)
    def description = _items.map(_.description).mkString(",")
    def add(item: Item) = _items :+= item
  }


  class Point(val x: Double, val y: Double)

  class LabeledPoint(val label: String, x: Double, y: Double) extends Point(x,y)

  class PointValue(private val coordinates: Long) extends AnyVal {
    def x = coordinates >> 32
    def y = coordinates >> 32
    def packed = (x << 32) | (y & 0xFFFFFFFFL)
    override def toString = f"${x}x${y}"
  }

  object PointValue {
    def apply(x: Int, y: Int) = new PointValue((x.toLong << 32) | (y & 0xFFFFFFFFL))
  }
}

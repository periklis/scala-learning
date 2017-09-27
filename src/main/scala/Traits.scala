package experiments

package object traits extends App {

  // var egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike

  // egg.translate(10, -10)
  // assert( egg.getX == 15)
  // assert( egg.getY == 20)

  // egg.grow(10,20)
  // assert( egg.getWidth == 30)
  // assert( egg.getHeight == 50)

  // var point1 = new OrderedPoint(1, 2)
  // var point2 = new OrderedPoint(1, 3)
  // var point3 = new OrderedPoint(0, 1)

  // assert(point1 < point2 == true)
  // assert(point1 > point3 == true)

  var logger = new {val test = ""} with ConsoleLogger
  logger.log("Test")

  var clogger = new {val test= ""; override val key = 9} with ConsoleLogger with CryptoLogger
  clogger.log("Test")

  var pointWithListener = new ListenerPoint(1, 1)
  pointWithListener.addPropertyChangeListener(new Listener())
  pointWithListener.setLocation(new java.awt.Point(2,2))

  val fis = new java.io.FileInputStream(".ensime") with Buffering
  fis.read()
  fis.close()

  val lfis = new java.io.FileInputStream(".ensime") with Buffering
  lfis.log("Test")
  lfis.close()

  val ifis = new java.io.FileInputStream(".ensime") with IterableInputStream
  println(ifis.map(_.toChar).mkString)
}

package traits {

  trait RectangleLike {
    // Add self type to use the shape of the conrecte instance where the trait gets mixed into
    this: java.awt.geom.Ellipse2D =>

    def translate(dx: Int, dy: Int): Unit = {
      this.getBounds2D().getBounds().translate(dx, dy)
    }
    def grow(h: Int, v: Int): Unit = {
      this.getBounds2D.getBounds.grow(h, v)
    }
  }

  class OrderedPoint(x: Int, y: Int) extends java.awt.Point(x, y) with math.Ordered[OrderedPoint] {
    def compare(that: OrderedPoint) =
      if (this.x == that.x) this.y - that.y
      else this.x - that.x
  }

  trait Logger {
    def log(msg: String): Unit
  }

  trait ConsoleLogger extends Logger {
    def log(msg: String): Unit = {println(msg)}
  }

  trait CryptoLogger extends Logger {
    val key = 3

    abstract override def log(msg: String): Unit = {
      val newShftAmt = key % 26
      val charArray = msg.toUpperCase().toCharArray()
      var ret = ""

      for (i<-charArray) {
        if (i.toInt == 32) {
          ret += " ";
        } else {
          var temp = (i.toInt - 65 + newShftAmt) % 26
          if (temp < 0) {
            temp += 26
          }
          ret += (temp + 65).toChar
        }
      }
      super.log(ret)
    }
  }

  trait PropertyChangeSupport {
    val pcs = new java.beans.PropertyChangeSupport(this);

    def addPropertyChangeListener(listener: java.beans.PropertyChangeListener ): Unit = {
      this.pcs.addPropertyChangeListener(listener);
    }

    def removePropertyChangeListener(listener: java.beans.PropertyChangeListener): Unit = {
      this.pcs.removePropertyChangeListener(listener);
    }
  }

  class Listener extends java.beans.PropertyChangeListener {
    def propertyChange(evt: java.beans.PropertyChangeEvent): Unit =  {
      println(evt.getSource.asInstanceOf[java.awt.Point].x)
      println(evt.getSource.asInstanceOf[java.awt.Point].y)
      println(evt.getNewValue.asInstanceOf[java.awt.Point].x)
      println(evt.getNewValue.asInstanceOf[java.awt.Point].y)
    }
  }

  class ListenerPoint(x: Int, y: Int) extends OrderedPoint(x,y) with PropertyChangeSupport {
    override def setLocation(p: java.awt.Point): Unit = {
      this.pcs.firePropertyChange("Location changed", this.getLocation, p.getLocation)
      super.setLocation(p)
    }
  }

  trait Component
  trait Container extends Component

  abstract class JComponent extends Component
  abstract class JContainer extends JComponent with Container

  class JButton extends JComponent
  class JPanel extends JContainer

  trait Buffering extends Logger {
    this: java.io.InputStream =>
    val buffer = new java.io.BufferedInputStream(this)

    override def read(): Int = {
      log("Diff Available : " + (buffer.available - this.available))
      buffer.read()
    }

    override def log(msg: String): Unit = {
      println(msg)
    }
  }


  trait IterableInputStream extends Iterable[Byte] {
    that: java.io.InputStream =>

    def iterator = new Iterator[Byte] {
      def hasNext = that.available() > 0
      def next() = that.read().toByte
    }
  }
}

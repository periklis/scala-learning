package experiments

package object advancedtypes extends App {

  new Bug().move(4).show().move(6).show().turn().move(5).show()
}

package advancedtypes {

  class Bug() {
    private var turned = false
    private var position = 0

    def move(p: Int): this.type = {
      position = if (!turned) position + p else position - p
      this
    }

    def turn(): this.type = {
      turned = true
      this
    }

    def show(): this.type = {
      println(position)
      this
    }
  }
}

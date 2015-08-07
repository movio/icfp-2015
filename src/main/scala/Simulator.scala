class Simulator(p: Problem, seedIndex: Int) {

  val board = Array.ofDim[Boolean](p.width, p.height)
  p.filled foreach (point ⇒ board(point.x)(point.y) = true)

  val source: Source = p.getSource(seedIndex)
  var current: Block = null

  def spawn(): Unit = {
    //if (current != null) throw new RuntimeException("current piece is still in play!!!!")
    val next = source.next

    val xs = next.members map (_.x)
    val min = xs.min
    val max = xs.max

    val left = (p.width - (max - min + 1)) / 2
    val xoffset = left - min
    val yoffset = next.members.map(_.y).min

    current = Block(
      next.members map (p ⇒ Point(p.x + xoffset, p.y - yoffset)),
      Point(next.pivot.x + xoffset, next.pivot.y - yoffset))
  }

  def draw(): Unit = {
    val width = p.width
    val height = p.height

    def char(x: Int, y: Int): String =
      if (board(x)(y)) "#"
      else if (current != null) {
        if (current.members contains (Point(x, y))) {
          if (current.pivot.x == x && current.pivot.y == y) "0"
          else "O"
        }
        else if (current.pivot.x == x && current.pivot.y == y) "X"
        else " "
      }
      else " "

    def drawEvenRow(y: Int): Unit = {
      print("/")
      print(" V" * (width - 1))
      if (y == 0) print(" \\") else print(" V")
      println()

      for (x ← 0 until width) {
        print("|" + char(x, y))
      }
      println("|")
    }

    def drawOddRow(y: Int): Unit = {
      print(" V" * width)
      println(" \\")

      print(" ")
      for (x ← 0 until width) {
        print("|" + char(x, y))
      }
      println("|")
    }

    for (y ← 0 until height) {
      if (y % 2 == 0) drawEvenRow(y)
      else drawOddRow(y)
    }
    if (height % 2 == 0) print(" ")
    println(" V" * width)
  }
}

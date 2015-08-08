sealed abstract trait Move

case object East extends Move
case object West extends Move
case object SouthEast extends Move
case object SouthWest extends Move
case object Clock extends Move
case object CounterClock extends Move

class Simulator(p: Problem, seedIndex: Int) {

  // TODO optimise this to (y, x) - makes line clearing much easier
  val board = Array.ofDim[Boolean](p.width, p.height)
  p.filled foreach (point ⇒ board(point.x)(point.y) = true)

  val source: Source = p.getSource(seedIndex)
  var current: Block = null
  spawn()

  var totalScore = 0

  var linesCleared = 0
  var linesClearedOld = 0

  private def spawn(): Simulator = {
    // TODO check game end
    val next = source.next

    val xs = next.members map (_.x)
    val ys = next.members map (_.y)
    val xmin = xs.min
    val xmax = xs.max
    val ymin = ys.min

    val left = (p.width - (xmax - xmin + 1)) / 2
    val xoffset = left - xmin
    val yoffset = -ymin

    current = Block(
      next.members map (p ⇒ Point(p.x + xoffset, p.y + yoffset)),
      Point(next.pivot.x + xoffset, next.pivot.y + yoffset))

    this
  }

  def play(move: Move): Simulator = {
    val next = current.move(move)

    // check for invalid move
    if (isLocationInvalid(next)) {
      lock()
      clearLines()
      score()
      spawn()
    } else {
      current = next
    }

    this
  }

  // TODO: check for collisions with filled cells
  private def isLocationInvalid(b: Block): Boolean =
    b.members exists (point ⇒ point.x < 0 || point.x >= p.width || point.y < 0 || point.y >= p.height)

  private def clearLines(): Simulator = {
    def isLineFull(y: Int): Boolean =
      (0 until p.width) forall (x ⇒ board(x)(y) == true)

    // the top row (y=0) is left dirty
    def clearLine(y: Int): Unit = {
      for {
        y ← (0 until y).reverse
        x ← 0 until p.width
      } {
        board(x)(y+1) = board(x)(y)
      }
    }

    linesClearedOld = linesCleared
    linesCleared = 0

    (0 until p.height) foreach { y ⇒
      if (isLineFull(y)) {
        clearLine(y)
        linesCleared += 1
      }
    }

    // if we cleared any lines, the top row (y=0) needs to be reset
    if (linesCleared > 0)
      (0 until p.width) foreach (x ⇒ board(x)(0) = false)

    this
  }

  private def score(): Simulator = {
    val size = current.members.size
    val points = size + (100 * (1 + linesCleared) * linesCleared / 2)
    val lineBonus = if (linesClearedOld > 1) (linesClearedOld - 1) * points / 10 else 0
    totalScore += points + lineBonus

    // TODO power bonus

    this
  }

  private def lock(): Simulator = {
    current.members.foreach (point ⇒ board(point.x)(point.y) = true)
    this
  }

  def draw(): Simulator = {
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

    this
  }
}

import collection.mutable

sealed abstract class Move(val s:String)
object Move {
  val all = Seq(SouthEast, SouthWest, East, West, Clock, CounterClock)
  def fromName(s: String): Move =
    all.find(_.s == s).get
}

sealed abstract class Rotate(s: String) extends Move(s)

case object West extends Move("p")
case object East extends Move("b")
case object SouthWest extends Move("a")
case object SouthEast extends Move("l")

case object Clock extends Rotate("d")
case object CounterClock extends Rotate("k")

object Simulator {
  def calculateSpawnLocation(block: Block, boardWidth: Int): Block = {
    val xs = block.members map (_.x)
    val ys = block.members map (_.y)
    val xmin = xs.min
    val xmax = xs.max
    val ymin = ys.min

    val left = (boardWidth - (xmax - xmin + 1)) / 2
    val xoffset = left - xmin
    val yoffset = -ymin

    val offset = Point(xoffset, yoffset)

    Block(
      block.members map (p ⇒ p.untranslate(offset)),
      block.pivot.untranslate(offset))
  }

  def isLocationInvalid(block: Block, board: Array[Array[Boolean]]): Boolean = {
    block.members exists { point ⇒
      point.x < 0 || point.x >= board.length || point.y < 0 || point.y >= board(0).length || board(point.x)(point.y)
    }
  }
}

class Simulator(p: Problem, seedIndex: Int) {
  import Simulator._

  // TODO optimise this to (y, x) - makes line clearing much easier
  val board = Array.ofDim[Boolean](p.width, p.height)
  p.filled foreach (point ⇒ board(point.x)(point.y) = true)

  private var source: Stream[Block] = p.createSource(seedIndex).asStream
  def getSource: Stream[Block] = source

  var current: Block = null
  private val history: mutable.Set[Block] = mutable.Set.empty[Block]
  spawn()

  var totalScore = 0

  private var linesCleared = 0
  private var linesClearedOld = 0

  var isGameOver = false

  private def spawn(): Simulator = {
    if (!source.isEmpty) {
      val next = source.head
      source = source.tail

      current = calculateSpawnLocation(next, p.width)

      history.clear()
      history.add(current)

      if (isLocationInvalid(current, board))
        gameOver()
    } else {
      gameOver()
    }

    this
  }

  private def gameOver(): Unit = {
    current = null
    isGameOver = true
  }

  def play(move: Move): Simulator = {
    if(!isGameOver) {
      val next = current.move(move)

      // error to repeat position
      if (history.contains(next)) {
        totalScore = 0
        gameOver()
      }

      // check for invalid move
      if (isLocationInvalid(next, board)) {
        lock()
        clearLines()
        score()
        spawn()
      } else {
        current = next
        history.add(current)
      }
    }

    this
  }

  def validCurrentPermutations(): Set[Block] =
    current.permutations(p.width, p.height).filterNot(isLocationInvalid(_, board))

  def playAll(s: String): Simulator = {
    val moves = s map (c ⇒ Move.fromName(c.toString))
    moves.foldLeft(this) { (s, move) ⇒
      val s2 = s.play(move).draw()
      readLine()
      s2
    }
  }

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

    println(s"Score: $totalScore")
    println()

    if (isGameOver) println("GAME OVER")

    this
  }
}

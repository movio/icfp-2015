import collection.mutable
import spray.json._

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

case class Solution(problemId: Int, seed: Int, tag: String, solution: String)
object Solution {
  import DefaultJsonProtocol._
  implicit val jf = jsonFormat4(Solution.apply)
}

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

  def draw(board: Array[Array[Boolean]], current: Block = null): Unit = {
    val width = board.length
    val height = board(0).length

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

class Simulator(p: Problem, seedIndex: Int, fitnessEvaluator: FitnessEvaluator = TotalFitness)  {
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

  def autoplay(): Simulator = {
    if (!isGameOver) {
      val moves = nextMoves()

      playAll((moves map (_.s)).mkString)


      // because it must exist, obviously /s
      val m = Move.all.find(move => isLocationInvalid(current.move(move), board)).get

      play(m)
      draw()
    }

    this
  }

  def quietAutoplay() = {
    if (!isGameOver) {
      val moves = nextMoves()

      quietPlayAll((moves map (_.s)).mkString)

      // because it must exist, obviously /s
      val m = Move.all.find(move => isLocationInvalid(current.move(move), board)).get

      play(m)
    }

    this
  }

  def quietPlayAll(s: String): Simulator = {
    val moves = s map (c ⇒ Move.fromName(c.toString))
    moves.foldLeft(this) { (s, move) ⇒
      s.play(move)
    }
  }

  def createSolution(): Solution = {
    val commands = new StringBuilder

    def placeBlock(): Unit = {
      val moves = nextMoves()
      moves foreach play
      // because it must exist, obviously /s
      val m = Move.all.find(move => isLocationInvalid(current.move(move), board)).get
      play(m)

      commands.append((moves map (_.s)).mkString + m.s)
    }

    while (!isGameOver) {
      placeBlock()
    }

    Solution(p.id, p.sourceSeeds(seedIndex), "alpha", commands.toString)
  }

  def output(): Simulator = {
    println("[" + createSolution().toJson.prettyPrint + "]")
    this
  }

  def nextMoves(): Seq[Move] =
    lockableCurrentPermutations().toStream.map { block ⇒
      val newBoard = board.map(_.clone())
      block.members.foreach(point ⇒ newBoard(point.x)(point.y) = true)
      (fitnessEvaluator(newBoard), block)
    }.sortBy(_._1).reverse.flatMap { case (_, target) ⇒

      // current == spawn location
      //Pathfinder.find(board, target, current)
      Pathfinder.astar(board, target, current)
    }.head

  def canBeLockedByOneMove(block: Block): Boolean =
    Move.all.exists(move => isLocationInvalid(block.move(move), board))

  def lockableCurrentPermutations(): Set[Block] =
    validCurrentPermutations().filter(canBeLockedByOneMove)

  def validCurrentPermutations(): Set[Block] =
    current.permutations(p.width, p.height).filterNot(isLocationInvalid(_, board))

  def playAll(s: String): Simulator = {
    val moves = s map (c ⇒ Move.fromName(c.toString))
    moves.foldLeft(this) { (s, move) ⇒
      val s2 = s.play(move).draw()
      //readLine()
      Thread.sleep(100)
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

    Simulator.draw(board, current)
    TotalFitness.printFitnessScores(board)
    println(s"Score: $totalScore")
    println()

    if (isGameOver) println("GAME OVER")

    this
  }
}


// BIGGER is always BETTER
class AggregateDepthFitness(weight: Double) extends Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = {
    board.foldLeft(0d) { (depth, col) ⇒      val firstIdx = col.indexOf(true)
      if (firstIdx == -1) {
        depth + col.length
      } else {
        depth + firstIdx
      }
    } * weight
  }
}

class CompleteLinesFitness(weight: Double) extends Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = {
    board.transpose.foldLeft(0) { (numCleared, row) ⇒      if (row.forall(b => b)) {
        numCleared + 1      } else {
        numCleared
      }
    } * weight
  }
}

class HoleFitness(weight: Double) extends Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = {
    board.foldLeft(0) { (count, col) =>
      val firstCovered = col.indexOf(true)
      if (firstCovered >= 0) {
        col.splitAt(firstCovered)._2.count(_ == false) + count
      } else {
        count
      }
    } * (-weight)
  }
}


class BumpinessFitness(weight: Double) extends Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = {
    if (board.length < 2) {
      return 0    }
    board.sliding(2).foldLeft(0) { (count, colWindow) =>
      val List(first,second) = colWindow.toList
      val firstHeight = substituteNegativeForLength(first.indexOf(true), first.length)
      val secondHeight = substituteNegativeForLength(second.indexOf(true), first.length)
      Math.abs(secondHeight - firstHeight) + count
    } * (-weight)
  }
  private def substituteNegativeForLength(idx:Int, len:Int): Int= {
    if (idx < 0) {
      len
    } else {
      idx
    }
  }
}

class LineFullnessFitness(weight: Double) extends Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = {
    val transposedBoard = board.transpose

    transposedBoard.foldLeft(0) { (count, row) ⇒      val rowCount = row.sliding(2).foldLeft(0) { (rowCount, window) =>
        val List(first,second) = window.toList
        if(first && second) 1 + rowCount else rowCount
      }
      (rowCount*rowCount) + count
    } * weight
  }
}

class SnugnessFitness(weight: Double) extends Function[Array[Array[Boolean]], Double] {

  val even = Seq((-1, -1), (0, -1), (-1, 0), (1, 0), (-1, 1), (0, 1))
  val odd = Seq((0, -1), (1, -1), (-1, 0), (1, 0), (0, 1), (1, 1))

  override def apply(board: Array[Array[Boolean]]): Double = {
    val width = board.length
    val height = board(0).length

    var cellCount = 0
    var neighbourCount = 0

    for {
      x ← 0 until width
      y ← 0 until height
      (dx, dy) ← if (y % 2 == 0) even else odd
    } {
      if (board(x)(y)) {
        cellCount += 1

        val px = x + dx
        val py = x + dy
        if (px < 0 || px >= width || py < 0 || py >= height || board(px)(py))
          neighbourCount += 1
      }
    }

    (neighbourCount / 6.0 / cellCount) * weight
  }
}

class StackingFitness(weight:Double) extends  Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = 1
}

class FitnessEvaluator(aggregateWeight: Double, bumpinessWeight: Double,
  completeLinesWeight: Double, holesWeight: Double, fullnessWeight: Double,
  snugnessWeight: Double) {

  val depth = new AggregateDepthFitness(aggregateWeight)
  val bumps = new BumpinessFitness(bumpinessWeight)
  val lines = new CompleteLinesFitness(completeLinesWeight)
  val holes = new HoleFitness(holesWeight)
  val fullness = new LineFullnessFitness(fullnessWeight)
  val snugness = new SnugnessFitness(snugnessWeight)

  def apply(board: Array[Array[Boolean]]): Double = {
    depth(board) + lines(board) + holes(board) + bumps(board) + fullness(board) + snugness(board)
  }

  def printFitnessScores(board: Array[Array[Boolean]]) = {
    println("Depth score: " + depth(board))
    println("Lines score: " + lines(board))
    println("Holes score: " + holes(board))
    println("Bumps score: " + bumps(board))
    println("Fullness score: " + fullness(board))
    println("Snugness score: " + snugness(board))
  }
}

object TotalFitness extends FitnessEvaluator(1,1,1,1,1,1)



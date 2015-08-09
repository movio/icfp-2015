
// BIGGER is always BETTER

class AggregateDepthFitness(weight: Double) extends Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = {
    board.foldLeft(0d) { (depth, col) ⇒
      val firstIdx = col.indexOf(true)
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
    board.transpose.foldLeft(0) { (numCleared, row) ⇒
      if (row.forall(b => b)) {
        numCleared + 1
      } else {
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
      return 0
    }
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
    board.transpose.foldLeft(0) { (count, row) ⇒
      row.sliding(2).foldLeft(0) { (rowCount, window) =>
        val List(first,second) = window.toList
        if(first && second) 1 + rowCount else rowCount
      } + count
    } * weight
  }
}

class StackingFitness(weight:Double) extends  Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = 1
}

object TotalFitness {
  val depth = new AggregateDepthFitness(1)
  val lines = new CompleteLinesFitness(10)
  val holes = new HoleFitness(0.2)
  val bumps = new BumpinessFitness(10)
  val fullness = new LineFullnessFitness(5)

  def apply(board: Array[Array[Boolean]]): Double = {
    depth(board) + lines(board) + holes(board) + bumps(board) + fullness(board)
  }
  
  def printFitnessScores(board: Array[Array[Boolean]]) = {
    println("Depth score: " + depth(board))
    println("Lines score: " + lines(board))
    println("Holes score: " + holes(board))
    println("Bumps score: " + bumps(board))
    println("Fullness score: " + fullness(board))
  }
}

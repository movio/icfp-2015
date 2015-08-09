
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

class StackingFitness(weight:Double) extends  Function[Array[Array[Boolean]], Double] {
  override def apply(board: Array[Array[Boolean]]): Double = 1}

class FitnessEvaluator(aggregateWeight: Double, bumpinessWeight: Double, 
  completeLinesWeight: Double, holesWeight: Double, fullnessWeight: Double) {

  val depth = new AggregateDepthFitness(aggregateWeight)
  val bumps = new BumpinessFitness(bumpinessWeight)
  val lines = new CompleteLinesFitness(completeLinesWeight)
  val holes = new HoleFitness(holesWeight)
  val fullness = new LineFullnessFitness(fullnessWeight)

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

object TotalFitness extends FitnessEvaluator(1,100,0.5,1,1) { 

}



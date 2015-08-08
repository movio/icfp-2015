import org.jgap.{ FitnessFunction, IChromosome }

class CommandFitness(problem: Problem) extends FitnessFunction {
  override def evaluate(chromosome: IChromosome): Double = {
    val builder = new StringBuilder
    val simulator = new Simulator(problem, 0)
    var i = 1
    chromosome.getGenes foreach { g ⇒
      if (!simulator.isGameOver) {
        simulator.play(Move.fromName(g.getAllele.toString))
        i += 1
        builder.append(g.getAllele.toString)
      }
    }

    simulator.totalScore
  }
}

// BIGGER is always BETTER

class AggergateDepthFitness(weight: Double) extends Function[Array[Array[Boolean]], Double] {
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


class BumpinessFintness(weight: Double) extends Function[Array[Array[Boolean]], Double] {
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

object TotalFitness {
  val depth = new AggergateDepthFitness(1)
  val lines = new CompleteLinesFitness(1)
  val holes = new HoleFitness(1)
  val bumps = new BumpinessFintness(1)

  def apply(board: Array[Array[Boolean]]): Double = {
    depth(board) + lines(board) + holes(board) + bumps(board)
  }
}

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
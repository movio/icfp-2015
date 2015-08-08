import org.jgap.{FitnessFunction, IChromosome}

class CommandFitness(problem:Problem) extends FitnessFunction {
  override def evaluate(chromosome: IChromosome): Double = {
    val builder = new StringBuilder
    val simulator = new Simulator(problem, 0)
    var i = 1
    chromosome.getGenes foreach { g =>
      if(!simulator.isGameOver) {
        simulator.play(Move.fromName(g.getAllele.toString))
        i += 1
        builder.append(g.getAllele.toString)
      }
    }

    simulator.totalScore
  }
}

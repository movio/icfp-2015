import ga._
import org.jgap.impl.DefaultConfiguration
import org.jgap.{Chromosome, Genotype}


object Start extends App {
  val conf = new DefaultConfiguration()
  val sampleGene = new CommandGene(conf, BaseCommandGenes.BasicCommands)
  sampleGene.setAllele("a")

  val problem = Problems.load(0)

  val maxPossibleValidRotations = 5
  val maxCommandSequence = problem.sourceLength * problem.height * problem.width * maxPossibleValidRotations
  conf.setFitnessFunction(new CommandFitness(problem))
  val chromosome = new Chromosome(conf, sampleGene, maxCommandSequence)
  conf.setSampleChromosome(chromosome)

  conf.setPopulationSize(100)

  val population = Genotype.randomInitialGenotype( conf );
  println("starting evolution")
  population.evolve(50)

  val fittest = population.getFittestChromosome
  val score = fittest.getFitnessValue
  val geneStr = fittest.getGenes.mkString("")

  val builder = new StringBuilder
  val simulator = new Simulator(problem, 0)
  fittest.getGenes foreach { g =>
    if(!simulator.isGameOver) {
      simulator.play(Move.fromName(g.getAllele.toString))
      builder.append(g.getAllele.toString)
    }
  }

  println(s"Fittest is: ${builder.toString}, with score: $score")
}
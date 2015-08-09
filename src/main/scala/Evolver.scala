import org.jgap.{IChromosome, FitnessFunction, Chromosome, Genotype, Gene}
import org.jgap.impl.{DoubleGene, DefaultConfiguration}

object Evolver extends App {

  val problem = Problems.load(0)
}

object FitnessEvolver {
  def evolve(problem: Problem) = {
    val conf = new DefaultConfiguration()

    val sampleGenes = Array[Gene](
      new DoubleGene(conf, -100, 100), // aggregate
      new DoubleGene(conf, -100, 100), // bumpiness
      new DoubleGene(conf, -100, 100), // completeLines
      new DoubleGene(conf, -100, 100), // holes
      new DoubleGene(conf, -100, 100) // fullness
    )

    val chromosome = new Chromosome(conf, sampleGenes)
    conf.setFitnessFunction(new WeightFitness(problem))
    conf.setSampleChromosome(chromosome)
    conf.setPopulationSize(100)
    val population = Genotype.randomInitialGenotype( conf );
    println("starting evolution")
    population.evolve(13)  

    population.getFittestChromosome
  }
}

class WeightFitness(problem: Problem) extends FitnessFunction {
  override def evaluate(iChromosome: IChromosome): Double = {

    val aggregate = iChromosome.getGenes()(0).getAllele.asInstanceOf[Double]
    val bumpiness = iChromosome.getGenes()(1).getAllele.asInstanceOf[Double]
    val completeLines = iChromosome.getGenes()(2).getAllele.asInstanceOf[Double]
    val holes = iChromosome.getGenes()(3).getAllele.asInstanceOf[Double]
    val fullness = iChromosome.getGenes()(4).getAllele.asInstanceOf[Double]

    val evaluator = new FitnessEvaluator(aggregate,bumpiness,completeLines,holes,fullness)

    val seedScores = (0 until problem.sourceSeeds.length) map { idx:Int => 
      val simulator = new Simulator(problem, idx)
      simulator.quietAutoplay
      simulator.totalScore
    } 

    seedScores.sum
  }
}
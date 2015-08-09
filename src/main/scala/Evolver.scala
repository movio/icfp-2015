
import org.jgap.audit.FitnessImprovementMonitor
import org.jgap._
import org.jgap.impl.{WeightedRouletteSelector, DoubleGene, DefaultConfiguration}

object Evolver {
  def evolveAll() = {
    (0 to 23).par foreach { i =>
      println(s"// Evolving for problem $i")
      val problem = Problems.load(i)
      val fitteset = FitnessEvolver.evolve(problem)
      val aggregate = fitteset.getGenes()(0).getAllele.asInstanceOf[Double]
      val bumpiness = fitteset.getGenes()(1).getAllele.asInstanceOf[Double]
      val completeLines = fitteset.getGenes()(2).getAllele.asInstanceOf[Double]
      val holes = fitteset.getGenes()(3).getAllele.asInstanceOf[Double]
      val fullness = fitteset.getGenes()(4).getAllele.asInstanceOf[Double]
      val snuggness = fitteset.getGenes()(4).getAllele.asInstanceOf[Double]

      println(s"// Problem $i: Got fittest with score: ${fitteset.getFitnessValue}")
      println(s"// Problem $i: Weight for aggregate: $aggregate")
      println(s"// Problem $i: Weight for bumpiness: $bumpiness")
      println(s"// Problem $i: Weight for completeLines: $completeLines")
      println(s"// Problem $i: Weight for holes: $holes")
      println(s"// Problem $i: Weight for fullness: $fullness")
      println(s"// Problem $i: Weight for snuggness: $fullness")
      println(s"val evolver_p$i = new FitnessEvaluator(aggregate, bumpiness, completeLines, holes, fullness, snuggness)")



      println(s"// End of evolution for problem $i")
      println()
      println()
    }
  }
}



object FitnessEvolver {
  def evolve(problem: Problem) = {
    Configuration.reset()
    val conf = new DefaultConfiguration()

    val sampleGenes = Array[Gene](
      new DoubleGene(conf, 0, 10), // aggregate
      new DoubleGene(conf, 0, 10), // bumpiness
      new DoubleGene(conf, 0, 10), // completeLines
      new DoubleGene(conf, 0, 10), // holes
      new DoubleGene(conf, 0, 10), // fullness
      new DoubleGene(conf, 0, 10) // snuggness
    )

    val chromosome = new Chromosome(conf, sampleGenes)

    val rouletteWheel = new WeightedRouletteSelector(conf)
    conf.removeNaturalSelectors(true)
    conf.removeNaturalSelectors(false)
    conf.addNaturalSelector(rouletteWheel, false)

    conf.setKeepPopulationSizeConstant(true)

    conf.setPreservFittestIndividual(true)
    conf.setFitnessFunction(new WeightFitness(problem))
    conf.setSampleChromosome(chromosome)
    conf.setPopulationSize(100)

    val population = Genotype.randomInitialGenotype( conf );
    println("starting evolution")

    val fitnessMonitor = new FitnessImprovementMonitor(30, 5, 1)

    population.evolve(fitnessMonitor)


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
    val snuggness = iChromosome.getGenes()(5).getAllele.asInstanceOf[Double]

    val evaluator = new FitnessEvaluator(aggregate,bumpiness,completeLines,holes,fullness,snuggness)

    val idx = problem.sourceSeeds.length - 1
    val simulator = new Simulator(problem, idx, evaluator)
    while(!simulator.isGameOver) { simulator.quietAutoplay }
    simulator.totalScore

  }
}

/*

Fitness value:2759.0,
problem0:[
  aggregate=8.67005540815273
  bumpiness=4.397865979086141
  completeLines=7.2287692370121
  holes=3.1386345213950992
  fullness=6.794297361821583
],

Fitness value:900.0,
problem1:[
  aggregate=0.9800802468956382,
  bumpiness=0.31562717868723666,
  completeLines=5.000505221245655,
  holes=9.554985442065306,
  fullness=2.7712195927643304
],

Application data:null


 */



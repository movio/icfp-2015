object WeightedEvaluators {
  
  val evaluators = List()
    /*fitnessEvaluator0, fitnessEvaluator1,
    fitnessEvaluator9, fitnessEvaluator10, 
    fitnessEvaluator11, fitnessEvaluator21, 
    fitnessEvaluator4, fitnessEvaluator22, 
    fitnessEvaluator23, fitnessEvaluator8, 
    fitnessEvaluator13, fitnessEvaluator6, 
    fitnessEvaluator16, fitnessEvaluator17)*/
}


object FindBestFromEvaluators {

  def run(problem:Problem, idx:Int) = {
    val solutions = WeightedEvaluators.evaluators.par.map { evaluator =>

      val sim = new Simulator(problem, idx, evaluator)

      (sim.totalScore, WeightedEvaluators.evaluators.indexOf(evaluator), sim.createSolution())
    }

    solutions.toList.sortBy(_._1).reverse
  }

}
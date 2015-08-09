object WeightedEvaluators {

  val evaluators = List()
    /*fitnessEvaluator0, fitnessEvaluator1,
    fitnessEvaluator9, fitnessEvaluator10,
    fitnessEvaluator11, fitnessEvaluator21,
    fitnessEvaluator4, fitnessEvaluator22,
    fitnessEvaluator23, fitnessEvaluator8,
    fitnessEvaluator13, fitnessEvaluator6,
    fitnessEvaluator16, fitnessEvaluator17)*/

  val p0 = new FitnessEvaluator(5.716967715503797, 1.1595831258368416, 0.051950907212752195, 6.317954858790241, 8.89387777865565, 1.1569839464403175)
  val p1 = new FitnessEvaluator(8.400643411411917, 2.9134034991845015, 4.459750535799028, 5.74388455092297, 9.396998119358072, 1.6002632733913835)
  val p6 = new FitnessEvaluator(3.249441948868812, 1.3922573006295713, 1.488640934056683, 1.8185665061685763, 8.771276177000045, 5.672366088868915)
  val p9 = new FitnessEvaluator(7.285622626895183, 9.31168939816711, 1.8760075561246803, 9.459805261245899, 4.226361725437574, 1.2834556712541478)
  val p11 = new FitnessEvaluator(1.8992605993372969, 7.7207817884032295, 4.0324223968364095, 2.1363086568654834, 8.569032390125578, 5.393558494094245)
  val p15 = new FitnessEvaluator(5.539635461032823, 2.680379659420906, 7.838284831881415, 8.254012737862288, 2.9650853846173786, 1.4763218005596868)

  val evaluatorsById = Map[Int, FitnessEvaluator](
    0 → p0,
    1 → p1,
    6 → p6,
    9 → p9,
    11 → p11,
    15 → p15
  )
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

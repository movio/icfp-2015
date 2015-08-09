object WeightedEvaluators {

  /*val Problem0WeightFor_aggregate = 8.67005540815273
  val Problem0WeightFor_bumpiness = 4.397865979086141
  val Problem0WeightFor_completeLines =  7.2287692370121
  val Problem0WeightFor_holes = 3.1386345213950992
  val Problem0WeightFor_fullness = 	6.794297361821583
  val fitnessEvaluator0 = new FitnessEvaluator(Problem0WeightFor_aggregate, Problem0WeightFor_bumpiness, Problem0WeightFor_completeLines, Problem0WeightFor_holes, Problem0WeightFor_fullness)

  val Problem1WeightFor_aggregate = 0.9800802468956382
  val Problem1WeightFor_bumpiness = 0.31562717868723666
  val Problem1WeightFor_completeLines =  5.000505221245655
  val Problem1WeightFor_holes = 9.554985442065306
  val Problem1WeightFor_fullness = 	2.7712195927643304
  val fitnessEvaluator1 = new FitnessEvaluator(Problem1WeightFor_aggregate, Problem1WeightFor_bumpiness, Problem1WeightFor_completeLines, Problem1WeightFor_holes, Problem1WeightFor_fullness)

  val Problem9WeightFor_aggregate = 2.2920655694807044
  val Problem9WeightFor_bumpiness = 7.521056721288861
  val Problem9WeightFor_completeLines =  0.10296176083482322
  val Problem9WeightFor_holes = 0.6670597071084206
  val Problem9WeightFor_fullness = 	8.060271315744753
  val fitnessEvaluator9 = new FitnessEvaluator(Problem9WeightFor_aggregate, Problem9WeightFor_bumpiness, Problem9WeightFor_completeLines, Problem9WeightFor_holes, Problem9WeightFor_fullness)

  val Problem10WeightFor_aggregate = 2.409601248979337
  val Problem10WeightFor_bumpiness = 7.012267954925859
  val Problem10WeightFor_completeLines =  6.073347972656703
  val Problem10WeightFor_holes = 4.479377910541173
  val Problem10WeightFor_fullness =  9.912366479525797
  val fitnessEvaluator10 = new FitnessEvaluator(Problem10WeightFor_aggregate, Problem10WeightFor_bumpiness, Problem10WeightFor_completeLines, Problem10WeightFor_holes, Problem10WeightFor_fullness)

  val Problem11WeightFor_aggregate = 6.7747366345896225
  val Problem11WeightFor_bumpiness = 6.739019791868395
  val Problem11WeightFor_completeLines =  2.929867847998734
  val Problem11WeightFor_holes = 1.1505492329636213
  val Problem11WeightFor_fullness =  5.863793095248251
  val fitnessEvaluator11 = new FitnessEvaluator(Problem11WeightFor_aggregate, Problem11WeightFor_bumpiness, Problem11WeightFor_completeLines, Problem11WeightFor_holes, Problem11WeightFor_fullness)

  val Problem21WeightFor_aggregate = 7.804015545343271
  val Problem21WeightFor_bumpiness = 9.684612602807658
  val Problem21WeightFor_completeLines =  3.032808114520711
  val Problem21WeightFor_holes = 7.09912114642114
  val Problem21WeightFor_fullness =  6.730680341466834
  val fitnessEvaluator21 = new FitnessEvaluator(Problem21WeightFor_aggregate, Problem21WeightFor_bumpiness, Problem21WeightFor_completeLines, Problem21WeightFor_holes, Problem21WeightFor_fullness)

  val Problem4WeightFor_aggregate = 3.1036471889802266
  val Problem4WeightFor_bumpiness = 7.290172056334047
  val Problem4WeightFor_completeLines =  1.3537107950423266
  val Problem4WeightFor_holes = 1.409152391943359
  val Problem4WeightFor_fullness = 	1.6379539026402674
  val fitnessEvaluator4 = new FitnessEvaluator(Problem4WeightFor_aggregate, Problem4WeightFor_bumpiness, Problem4WeightFor_completeLines, Problem4WeightFor_holes, Problem4WeightFor_fullness)

  val Problem22WeightFor_aggregate = 6.50636956057352
  val Problem22WeightFor_bumpiness = 2.6102642143108277
  val Problem22WeightFor_completeLines =  2.9970258994441346
  val Problem22WeightFor_holes = 1.2301696354525804
  val Problem22WeightFor_fullness =  8.610318392047905
  val fitnessEvaluator22 = new FitnessEvaluator(Problem22WeightFor_aggregate, Problem22WeightFor_bumpiness, Problem22WeightFor_completeLines, Problem22WeightFor_holes, Problem22WeightFor_fullness)

  val Problem23WeightFor_aggregate = 5.6108746379951535
  val Problem23WeightFor_bumpiness = 7.332829344110701
  val Problem23WeightFor_completeLines =  8.0514055906543
  val Problem23WeightFor_holes = 3.0362531933217007
  val Problem23WeightFor_fullness =  1.1939394990649133
  val fitnessEvaluator23 = new FitnessEvaluator(Problem23WeightFor_aggregate, Problem23WeightFor_bumpiness, Problem23WeightFor_completeLines, Problem23WeightFor_holes, Problem23WeightFor_fullness)

  val Problem8WeightFor_aggregate = 7.207760728097075
  val Problem8WeightFor_bumpiness = 4.909750488849117
  val Problem8WeightFor_completeLines =  8.895566325164179
  val Problem8WeightFor_holes = 2.118339455881526
  val Problem8WeightFor_fullness = 	4.790025989045283
  val fitnessEvaluator8 = new FitnessEvaluator(Problem8WeightFor_aggregate, Problem8WeightFor_bumpiness, Problem8WeightFor_completeLines, Problem8WeightFor_holes, Problem8WeightFor_fullness)

  val Problem13WeightFor_aggregate = 2.701082589409891
  val Problem13WeightFor_bumpiness = 1.8801761123286487
  val Problem13WeightFor_completeLines =  9.15021871143013
  val Problem13WeightFor_holes = 5.137930259632455
  val Problem13WeightFor_fullness =  5.5546552091083266
  val fitnessEvaluator13 = new FitnessEvaluator(Problem13WeightFor_aggregate, Problem13WeightFor_bumpiness, Problem13WeightFor_completeLines, Problem13WeightFor_holes, Problem13WeightFor_fullness)

  val Problem6WeightFor_aggregate = 8.142017597515702
  val Problem6WeightFor_bumpiness = 3.5781212068305557
  val Problem6WeightFor_completeLines =  0.8813026155845527
  val Problem6WeightFor_holes = 3.8491301806078404
  val Problem6WeightFor_fullness = 	2.1024251481430287
  val fitnessEvaluator6 = new FitnessEvaluator(Problem6WeightFor_aggregate, Problem6WeightFor_bumpiness, Problem6WeightFor_completeLines, Problem6WeightFor_holes, Problem6WeightFor_fullness)

  val Problem16WeightFor_aggregate = 9.702931895114057
  val Problem16WeightFor_bumpiness = 5.460737953637338
  val Problem16WeightFor_completeLines = 3.9263917105060164
  val Problem16WeightFor_holes = 4.4943170684459535
  val Problem16WeightFor_fullness = 1.3393800043415582
  val fitnessEvaluator16 = new FitnessEvaluator(Problem16WeightFor_aggregate, Problem16WeightFor_bumpiness, Problem16WeightFor_completeLines, Problem16WeightFor_holes, Problem16WeightFor_fullness)

  val Problem17WeightFor_aggregate = 4.624041717087706
  val Problem17WeightFor_bumpiness = 7.178788842946414
  val Problem17WeightFor_completeLines = 9.941197291904842
  val Problem17WeightFor_holes = 0.12663264785833128
  val Problem17WeightFor_fullness = 8.21260008097342
  val fitnessEvaluator17 = new FitnessEvaluator(Problem17WeightFor_aggregate, Problem17WeightFor_bumpiness, Problem17WeightFor_completeLines, Problem17WeightFor_holes, Problem17WeightFor_fullness)
*/
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
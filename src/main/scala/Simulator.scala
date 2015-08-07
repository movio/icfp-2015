class Simulator(p: Problem, seedIndex: Int) {

  val board = Array.ofDim[Boolean](p.width, p.height)
  p.filled foreach (point ⇒ board(point.x)(point.y) = true)


}

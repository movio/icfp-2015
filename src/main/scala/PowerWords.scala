object PowerWords {

  //  {p, ', !, ., 0, 3}	move W
  //  {b, c, e, f, y, 2}	move E
  //  {a, g, h, i, j, 4}	move SW
  //  {l, m, n, o, space, 5}    	move SE
  //  {d, q, r, v, z, 1}	rotate clockwise
  //  {k, s, t, u, w, x}	rotate counter-clockwise
  //  \t, \n, \r	(ignored)

  val charToMove: Map[Char, Move] = Map(
    "p'!.03" -> West,
    "bcefy2" -> East,
    "aghij4" -> SouthWest,
    "lmno 5" -> SouthEast,
    "dqrvz1" -> Clock,
    "kstuwx" -> CounterClock
  ) flatMap { case (s, m) ⇒ s map (c ⇒ c -> m) }

  def toMoves(s: String): Seq[Move] = s.toLowerCase map charToMove

  def accept(s: String): Boolean = Moves.nonStuttering(toMoves(s))

}

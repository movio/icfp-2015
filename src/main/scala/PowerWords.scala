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

  def findValidEmbedding(moves: Seq[Move], embeddingsMap: Map[Seq[Move], Seq[Move]], isValidMove: (Seq[Move] => Boolean)): Seq[Move] = {
    def s(m: Seq[Move]): String = m.map(_.s).mkString
    var i = 0
    var done = false
    var current = moves
    while (!done) {
      done = true
      embeddingsMap foreach { case (embedding, powerMove) =>
        val pos = s(current.drop(i)).indexOf(s(embedding))
        if (pos != -1) {
          val prefix = current.take(i + pos)
          val suffix = current.drop(i + pos).drop(embedding.length)
          val candidate = prefix ++ powerMove ++ suffix
          if (isValidMove(candidate)) {
            done = false
            current = candidate
            i = prefix.length + powerMove.length
          }
        }
      }
    }
    current
  }

}

case class PowerWords(maxTimeMillis: Int) {

  val powerWords: Seq[String] = Seq(
    "Ei",
    "Ia! Ia",
    "r'lyeh",
    "cthulu",
    "davar",
    "old ones",
//    "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn",
//    "Cthulhu fhtagn",
    "Lovecraft",
    "Azathoth"
  )

  val powerMoves: Seq[Seq[Move]] = powerWords map PowerWords.toMoves

  val embeddingsMap: Map[Seq[Move], Seq[Move]] = {
    powerMoves flatMap { powerMove: Seq[Move] ⇒
      Moves.findINSS(powerMove, maxTimeMillis / powerWords.length) map (moves ⇒ moves -> powerMove)
    }
  }.toMap

}

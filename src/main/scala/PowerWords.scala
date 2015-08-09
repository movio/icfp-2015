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

  def findValidEmbedding(moves: Seq[Move], embeddingsMap: Map[Seq[Move], String], isValidMove: (Seq[Move] => Boolean)): String = {
    def s(m: Seq[Move]): String = m.map(_.s).mkString
    var i = 0
    var done = false
    var current = s(moves)
    while (!done) {
      done = true
      embeddingsMap foreach { case (embedding, powerWord) =>
        val pos = current.drop(i).indexOf(s(embedding))
        if (pos != -1) {
          val prefix = current.take(i + pos)
          val suffix = current.drop(i + pos).drop(embedding.length)
          val candidate = prefix ++ powerWord ++ suffix

          val candidateMoves = toMoves(candidate)
          if (isValidMove(candidateMoves) && Moves.nonStuttering(candidateMoves)) {
//            println("using power word " + powerWord + " with isomorphism " + embedding)
            done = false
            current = candidate
            i = prefix.length + powerWord.length
          }
        }
      }
    }
    current
  }

}

case class PowerWords(maxTimeMillis: Int) {

  val powerWords: Seq[String] = Seq(
    "ei!",
    "ia! ia!",
    "r'lyeh",
    "cthulu",
    "davar",
    "old ones",
//    "ph'nglui mglw'nafh cthulhu r'lyeh wgah'nagl fhtagn",
//    "cthulhu fhtagn",
    "lovecraft",
    "azathoth"
  )

  //val powerMoves: Seq[Seq[Move]] = powerWords map PowerWords.toMoves

  val embeddingsMap: Map[Seq[Move], String] = {
    powerWords flatMap { powerWord: String ⇒
      val equivalentMoves: Seq[Seq[Move]] =
        Moves.findINSS(PowerWords.toMoves(powerWord), maxTimeMillis / powerWords.length)
      val tuples: Seq[(Seq[Move], String)] = equivalentMoves map (moves ⇒ moves -> powerWord)
      tuples
    }
  }.toMap

  def findValidEmbedding(moves: Seq[Move], isValidMove: (Seq[Move] => Boolean)): String =
    PowerWords.findValidEmbedding(moves, embeddingsMap, isValidMove)

}

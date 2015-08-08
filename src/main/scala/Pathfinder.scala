import collection.mutable

object Pathfinder {

  case class Partial(location: Block, moves: List[Move])

  def find(board: Array[Array[Boolean]], target: Block, spawn: Block): Seq[Move] = {

    var history = mutable.Set.empty[Block]

    // find one step out and filter invalid locations
    def expand(p: Partial): Stream[Partial] = {
      history.add(p.location)

      (Move.all map {m ⇒
        val prevLocation = p.location.moveReverse(m)
        if (Simulator.isLocationInvalid(prevLocation, board))
          null
        else
          Partial(prevLocation, m :: p.moves)
      } filterNot (p ⇒ p == null || history.contains(p.location))).toStream
    }

    var stream = Stream(Partial(target, Nil)) // step 0

    val spawnLocation = Simulator.calculateSpawnLocation(spawn, board.length)
    //println(spawnLocation)

    var result = Seq.empty[Move]
    while (result.isEmpty && stream.headOption.isDefined) {
      //println("-------------------------")
      //stream foreach println
      stream find (_.location == spawnLocation) match {
        case Some(Partial(_, moves)) ⇒
          result = moves
        case _ ⇒
          stream = stream flatMap expand
      }
    }

    //result foreach println
    result
  }
}

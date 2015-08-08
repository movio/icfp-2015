import collection.mutable

object Pathfinder {

  case class Partial(location: Block, moves: List[Move])

  def find(board: Array[Array[Boolean]], target: Block, spawn: Block): Seq[Move] = {

    //println("finding path...")
    //println("looking at board: ")
    //Simulator.draw(board, target)
    //println("------------------------")

    var history = mutable.Set.empty[Block]
    history.add(target)

    // find one step out and filter invalid locations
    def expand(p: Partial): Stream[Partial] = {
      (Move.all flatMap {m ⇒
        val prevLocation = p.location.moveReverse(m)
        if (Simulator.isLocationInvalid(prevLocation, board) || history.contains(prevLocation))
          None
        else {
          history.add(prevLocation)
          Some(Partial(prevLocation, m :: p.moves))
        }
      }).toStream
    }

    var stream = Stream(Partial(target, Nil)) // step 0

    val spawnLocation = Simulator.calculateSpawnLocation(spawn, board.length)
    //println(spawnLocation)

    // TODO separate empty list from no solution
    var result = Seq.empty[Move]
    while (target != spawn && result.isEmpty && stream.headOption.isDefined) {
      //println("-------------------------")
      //stream foreach println
      stream find (_.location == spawnLocation) match {
        case Some(Partial(_, moves)) ⇒
          result = moves
        case _ ⇒
          stream = stream flatMap expand
      }
    }

    //println("---------")
    //println("found path:")
    //result foreach println
    //println("---------")
    result
  }
}

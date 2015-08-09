import collection.mutable

class AStarOrdering(spawnLocation: Block, knownBestScore: mutable.Map[Block, Double]) extends Ordering[Block] {
  def compare(a: Block, b: Block): Int = {
    val scoreA = knownBestScore.getOrElse(a, Double.PositiveInfinity) + Pathfinder.dist(a, spawnLocation)
    val scoreB = knownBestScore.getOrElse(b, Double.PositiveInfinity) + Pathfinder.dist(b, spawnLocation)

    // reversed - smaller is better
    Math.signum(scoreB - scoreA).asInstanceOf[Int]
  }
}

object Pathfinder {

  // TODO use cube coordinates for better estimate?
  def dist(a: Block, b: Block): Double = {
    val dx = a.pivot.x - b.pivot.x
    val dy = a.pivot.y - b.pivot.y
    Math.sqrt(dx * dx + dy * dy)
  }

  def astar(board: Array[Array[Boolean]], target: Block, spawn: Block): Option[Seq[Move]] = {
    val spawnLocation = Simulator.calculateSpawnLocation(spawn, board.length)

    val knownBestScore = mutable.Map.empty[Block, Double]
    knownBestScore(target) = 0

    val heuristic = new AStarOrdering(spawnLocation, knownBestScore)

    val visited = mutable.Set.empty[Block]
    val added = mutable.Set.empty[Block]
    val queue = mutable.PriorityQueue.empty[Block](heuristic)
    queue.enqueue(target)
    added.add(target)

    // given a block, which move takes it to the next best location
    val bestMove = mutable.Map.empty[Block, Move]

    // debug
    //var nLocationsSearched = 0

    while (queue.nonEmpty) {
      // debug
      //val all = queue.dequeueAll
      //println("--------------------")
      //all foreach {e ⇒ println(e); queue.enqueue(e)}
      //println("--------------------")
      //readLine()
      //nLocationsSearched += 1

      val current = queue.dequeue()

      // if we reached our destination, we're done
      if (current == spawnLocation) {
        val path = mutable.Buffer.empty[Move]
        var block = spawnLocation
        while (block != target) {
          val move = bestMove(block)
          block = block.move(move)
          path.append(move)
        }
        //println(s"locations searched: $nLocationsSearched")
        return Some(path)
      }

      // we haven't reached our destination - add this node to visited
      visited.add(current)

      // add all unvisited neighbours to queue, updating our best known score
      for (move ← Move.all) {
        val neighbour = current.moveReverse(move)
        if (!visited.contains(neighbour) && !Simulator.isLocationInvalid(neighbour, board)) {
          // includes a penalty for rotations
          val distEstimate = knownBestScore.getOrElse(current, Double.PositiveInfinity) +
            Pathfinder.dist(current, neighbour) + (if (move == Clock || move == CounterClock) 0.5 else 0)

          // if we find a better position
          if (distEstimate < knownBestScore.getOrElse(neighbour, Double.PositiveInfinity)) {
            bestMove(neighbour) = move
            knownBestScore(neighbour) = distEstimate

            if (!added.contains(neighbour)) {
              queue.enqueue(neighbour)
              added.add(neighbour)
            }
          }
        }
      }
    }

    None
  }

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

    // TODO separate empty list from no solution\
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

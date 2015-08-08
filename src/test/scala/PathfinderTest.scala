import org.scalatest._

class PathfinderTest extends FunSpec with ShouldMatchers {

  it("shortest path on empty board") {
    val board = Array.ofDim[Boolean](5, 5)
    val target = Block(Set(Point(2,4),Point(3,4)), pivot = Point(2,4))
    val spawn = Block(Set(Point(1,0),Point(2,0)), pivot = Point(1,0))

    Pathfinder.find(board, target, spawn) shouldBe Seq(SouthWest, SouthEast, SouthEast, SouthEast)
  }

  it("finds path on tricky board") {
    val board = Array.ofDim[Boolean](7, 8)
    Seq(
      (2,2),(1,3),(0,4),(1,4),
      (4,6),(3,7),
      (3,4),(4,4),(5,4),(5,5),(6,6),(5,7)
    ) foreach { case (x, y) ⇒ board(x)(y) = true }

    val target = Block(Set(Point(4,5),Point(5,6),Point(4,7)), pivot = Point(4,6))
    val spawn = Block(Set(Point(3,0),Point(3,1),Point(3,2)),Point(2,1))

    Pathfinder.find(board, target, spawn) shouldBe Seq(
      CounterClock, SouthEast, SouthEast, CounterClock, SouthWest,
      CounterClock, SouthWest, SouthEast, Clock, East, Clock, Clock
    )
  }

  it("finds path on tricky board with different spawn orientation") {
    val board = Array.ofDim[Boolean](7, 8)
    Seq(
      (2,2),(1,3),(0,4),(1,4),
      (4,6),(3,7),
      (3,4),(4,4),(5,4),(5,5),(6,6),(5,7)
    ) foreach { case (x, y) ⇒ board(x)(y) = true }

    val target = Block(Set(Point(4,5),Point(5,6),Point(4,7)), pivot = Point(4,6))
    val spawn = Block(Set(Point(1,0),Point(2,0),Point(0,1)),Point(1,1))

    Pathfinder.find(board, target, spawn) shouldBe Seq(
      SouthEast, SouthWest, SouthWest,
      CounterClock, SouthWest, SouthEast,
      Clock, East, Clock, Clock
    )
  }
}

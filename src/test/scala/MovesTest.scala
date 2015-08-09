import org.scalatest._

class MovesTest extends FunSpec with ShouldMatchers {

  it("should work") {
    Moves.ofLength(0).toSet shouldBe Set(
      Seq()
    )
    Moves.ofLength(1).toSet shouldBe Set(
      Seq(East), Seq(West), Seq(SouthEast), Seq(SouthWest), Seq(Clock), Seq(CounterClock)
    )
    Moves.ofLength(2).toSet shouldBe Set(
      Seq(SouthWest, West), Seq(East, East), Seq(CounterClock, CounterClock), Seq(CounterClock, SouthEast),
      Seq(SouthWest, East), Seq(East, SouthWest), Seq(East, CounterClock), Seq(West, East), Seq(East, West),
      Seq(Clock, Clock), Seq(SouthWest, Clock), Seq(SouthWest, CounterClock), Seq(West, West), Seq(West, SouthEast),
      Seq(Clock, CounterClock), Seq(West, CounterClock), Seq(East, Clock), Seq(SouthEast, SouthWest),
      Seq(SouthEast, SouthEast), Seq(Clock, East), Seq(CounterClock, West), Seq(Clock, SouthEast), Seq(Clock, West),
      Seq(SouthEast, East), Seq(SouthEast, West), Seq(West, Clock), Seq(East, SouthEast), Seq(CounterClock, East),
      Seq(SouthEast, CounterClock), Seq(Clock, SouthWest), Seq(SouthWest, SouthEast), Seq(CounterClock, Clock),
      Seq(CounterClock, SouthWest), Seq(SouthEast, Clock), Seq(SouthWest, SouthWest), Seq(West, SouthWest)
    )
  }

  it("computes isomorphisms") {
    val moves: Seq[Move] = PowerWords.toMoves("Ei!")
    Moves.isomorphicNonStutteringSequences(moves).take(20).toList shouldBe List(
      List(SouthWest),
      List(SouthEast, West),
      List(West, SouthEast),
      List(East, SouthWest, West),
      List(West, SouthWest, East),
      List(Clock, SouthWest, CounterClock),
      List(CounterClock, SouthWest, Clock),
      List(SouthEast, Clock, West, CounterClock),
      List(SouthEast, CounterClock, West, Clock),
      List(East, SouthEast, West, West),
      List(West, West, SouthEast, East),
      List(West, Clock, SouthEast, CounterClock),
      List(West, CounterClock, SouthEast, Clock),
      List(Clock, SouthEast, West, CounterClock),
      List(Clock, SouthEast, CounterClock, West),
      List(Clock, West, SouthEast, CounterClock),
      List(Clock, West, CounterClock, SouthEast),
      List(CounterClock, SouthEast, West, Clock),
      List(CounterClock, SouthEast, Clock, West),
      List(CounterClock, West, SouthEast, Clock)
    )
  }

  it("finds isomorphisms in bounded time") {
    val moves: Seq[Move] = PowerWords.toMoves("Cthulu")
    Moves.findINSS(moves, 1000).length shouldBe (484  +- 100)
    Moves.findINSS(moves, 2000).length shouldBe (964  +- 100)
    Moves.findINSS(moves, 3000).length shouldBe (1310 +- 100)
  }

}

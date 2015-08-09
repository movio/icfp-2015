import org.scalatest._

class PowerWordsTest extends FunSpec with ShouldMatchers {

  it("maps chars to moves") {
    PowerWords.charToMove shouldBe Map(
      'p' -> West,
      ''' -> West,
      '.' -> West,
      '!' -> West,
      '0' -> West,
      '3' -> West,
      'e' -> East,
      'y' -> East,
      'f' -> East,
      'b' -> East,
      '2' -> East,
      'c' -> East,
      'h' -> SouthWest,
      '4' -> SouthWest,
      'i' -> SouthWest,
      'j' -> SouthWest,
      'a' -> SouthWest,
      'g' -> SouthWest,
      '5' -> SouthEast,
      'm' -> SouthEast,
      ' ' -> SouthEast,
      'l' -> SouthEast,
      'o' -> SouthEast,
      'n' -> SouthEast,
      'r' -> Clock,
      'z' -> Clock,
      'd' -> Clock,
      '1' -> Clock,
      'q' -> Clock,
      'v' -> Clock,
      'w' -> CounterClock,
      't' -> CounterClock,
      'u' -> CounterClock,
      'k' -> CounterClock,
      's' -> CounterClock,
      'x' -> CounterClock
    )
  }

  it("accepts words") {
    PowerWords.accept("Ei!") shouldBe true
    PowerWords.accept("Ia! Ia!") shouldBe true
    PowerWords.accept("r'lyeh") shouldBe true
    PowerWords.accept("cthulu") shouldBe true
    PowerWords.accept("davar") shouldBe true
    PowerWords.accept("old ones") shouldBe true
    PowerWords.accept("Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn!") shouldBe true
    PowerWords.accept("Cthulhu fhtagn!") shouldBe true
    PowerWords.accept("Aleister") shouldBe true
    PowerWords.accept("Lovecraft") shouldBe true
    PowerWords.accept("Azathoth") shouldBe true
    PowerWords.accept("2xjw 4s") shouldBe true
  }

  it("translates into moves") {
    PowerWords.toMoves("Ei!") shouldBe Seq(East, SouthWest, West)
    PowerWords.toMoves("Ia! Ia!") shouldBe Seq(SouthWest, SouthWest, West, SouthEast, SouthWest, SouthWest, West)
    PowerWords.toMoves("r'lyeh") shouldBe Seq(Clock, West, SouthEast, East, East, SouthWest)
    PowerWords.toMoves("cthulu") shouldBe Seq(East, CounterClock, SouthWest, CounterClock, SouthEast, CounterClock)
    PowerWords.toMoves("davar") shouldBe Seq(Clock, SouthWest, Clock, SouthWest, Clock)
    PowerWords.toMoves("old ones") shouldBe Seq(SouthEast, SouthEast, Clock, SouthEast, SouthEast, SouthEast, East, CounterClock)
    PowerWords.toMoves("Lovecraft") shouldBe Seq(SouthEast, SouthEast, Clock, East, East, Clock, SouthWest, East, CounterClock)
    PowerWords.toMoves("Azathoth") shouldBe Seq(SouthWest, Clock, SouthWest, CounterClock, SouthWest, SouthEast, CounterClock, SouthWest)
    PowerWords.toMoves("2xjw 4s") shouldBe Seq(East, CounterClock, SouthWest, CounterClock, SouthEast, SouthWest, CounterClock)
  }


  it("rejects East West pairs") {
    PowerWords.accept("PE") shouldBe false
  }

  it("rejects less obvious bad words (East East Clock West West CounterClock)") {
    val badPowerWord = "eyrp!w"
    PowerWords.toMoves(badPowerWord) shouldBe Seq(East, East, Clock, West, West, CounterClock)
    PowerWords.accept(badPowerWord) shouldBe false
  }

  it("finds valid embeddings") {

    val moves = Seq(East, Clock, SouthWest, Clock, West, West)

    val embeddingsMap: Map[Seq[Move], Seq[Move]] =  Map(
      Seq(East, Clock) -> Seq(Clock, East),
      Seq(Clock, West) -> Seq(West, Clock, SouthEast, Clock)
    )

    def isValidMove: Seq[Move] => Boolean = x => true

    PowerWords.findValidEmbedding(moves, embeddingsMap, isValidMove) shouldBe  List(
      Clock, East, SouthWest, West, Clock, SouthEast, Clock, West
    )

  }

  it("finds valid embeddings that don't stutter") {

    val moves = Seq(East, Clock, West, Clock, West, West)

    val embeddingsMap: Map[Seq[Move], Seq[Move]] =  Map(
      Seq(East, Clock) -> Seq(Clock, East),
      Seq(West, Clock) -> Seq(West, Clock, SouthEast, Clock)
    )

    def isValidMove: Seq[Move] => Boolean = x => true

    PowerWords.findValidEmbedding(moves, embeddingsMap, isValidMove) shouldBe List(
      East, Clock, West, Clock, SouthEast, Clock, West, West
    )

  }

}

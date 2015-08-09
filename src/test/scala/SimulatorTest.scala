import org.scalatest._

class SimulatorTest extends FunSpec with ShouldMatchers {
  it("should move pieces East correctly") {
    Point(0, 0).move(East) shouldBe Point(1, 0)
    Point(0, 0).move(East).move(East) shouldBe Point(2, 0)
    Point(0, 1).move(East) shouldBe Point(1, 1)
    Point(0, 1).move(East).move(East) shouldBe Point(2, 1)
  }

  it("should move pieces West correctly") {
    Point(1, 0).move(West) shouldBe Point(0, 0)
    Point(1, 0).move(West).move(West) shouldBe Point(-1, 0)
    Point(1, 1).move(West) shouldBe Point(0, 1)
    Point(1, 1).move(West).move(West) shouldBe Point(-1, 1)
  }

  it("should move pieces SouthEast correctly") {
    Point(0, 0).move(SouthEast) shouldBe Point(0, 1)
    Point(0, 0).move(SouthEast).move(SouthEast) shouldBe Point(1, 2)
    Point(0, 1).move(SouthEast) shouldBe Point(1, 2)
    Point(0, 1).move(SouthEast).move(SouthEast) shouldBe Point(1, 3)
  }

  it("should move pieces SouthWest correctly") {
    Point(1, 0).move(SouthWest) shouldBe Point(0, 1)
    Point(1, 0).move(SouthWest).move(SouthWest) shouldBe Point(0, 2)
    Point(1, 1).move(SouthWest) shouldBe Point(1, 2)
    Point(1, 1).move(SouthWest).move(SouthWest) shouldBe Point(0, 3)
  }

  it("translate and untranslate and get to the same point") {
    Point(0, 0).translate(Point(0, 0)).untranslate(Point(0, 0)) shouldBe Point(0, 0)
    Point(0, 0).translate(Point(1, 1)).untranslate(Point(1, 1)) shouldBe Point(0, 0)
    Point(1, 1).translate(Point(0, 0)).untranslate(Point(0, 0)) shouldBe Point(1, 1)
    Point(1, 1).translate(Point(1, 1)).untranslate(Point(1, 1)) shouldBe Point(1, 1)
  }

  it("should translate correctly around a pivot on a even row") {
    Point(2, 2).translate(Point(2, 2)) shouldBe Point(0, 0)
    Point(2, 3).translate(Point(2, 2)) shouldBe Point(0, 1)
    Point(2, 4).translate(Point(2, 2)) shouldBe Point(0, 2)
  }

  it("should translate correctly around a pivot on an odd row") {
    Point(3, 4).translate(Point(2, 3)) shouldBe Point(0, 1)
    Point(3, 3).translate(Point(2, 3)) shouldBe Point(1, 0)
    Point(4, 4).translate(Point(2, 3)) shouldBe Point(1, 1)
  }

  it("should rotate with a relative (0,0) pivot") {
    Point(0, 0).move(Clock, Point(0, 0)) shouldBe Point(0, 0)
    Point(0, 1).move(Clock, Point(0, 1)) shouldBe Point(0, 1)
    Point(1, 0).move(Clock, Point(1, 0)) shouldBe Point(1, 0)
    Point(1, 1).move(Clock, Point(1, 1)) shouldBe Point(1, 1)
  }

  it("rotate big objects clockwise with an even row pivot") {
    Point(0, 3).move(Clock, Point(2, 4)) shouldBe Point(2, 2)
    Point(1, 3).move(Clock, Point(2, 4)) shouldBe Point(2, 3)
    Point(4, 2).move(Clock, Point(2, 4)) shouldBe Point(4, 5)
  }

  it("rotate big objects counter clockwise with an even row pivot") {
    Point(0, 3).move(CounterClock, Point(2, 4)) shouldBe Point(0, 5)
    Point(1, 3).move(CounterClock, Point(2, 4)) shouldBe Point(1, 4)
    Point(4, 2).move(CounterClock, Point(2, 4)) shouldBe Point(1, 1)
  }

  it("rotate big objects clockwise with an odd row pivot") {
    Point(0, 2).move(Clock, Point(1, 3)) shouldBe Point(1, 1)
    Point(1, 2).move(Clock, Point(1, 3)) shouldBe Point(2, 2)
    Point(3, 1).move(Clock, Point(1, 3)) shouldBe Point(4, 4)
  }

  it("rotate big objects counter clockwise with an odd row pivot") {
    Point(0, 2).move(CounterClock, Point(1, 3)) shouldBe Point(0, 4)
    Point(1, 2).move(CounterClock, Point(1, 3)) shouldBe Point(0, 3)
    Point(3, 1).move(CounterClock, Point(1, 3)) shouldBe Point(1, 0)
  }

  describe("reverse") {
    it("should move pieces East reverse correctly") {
      Point(1, 0).moveReverse(East) shouldBe Point(0, 0)
      Point(2, 0).moveReverse(East).moveReverse(East) shouldBe Point(0, 0)
      Point(1, 1).moveReverse(East) shouldBe Point(0, 1)
      Point(2, 1).moveReverse(East).moveReverse(East) shouldBe Point(0, 1)
    }

    it("should move pieces West reverse correctly") {
      Point(0, 0).moveReverse(West) shouldBe Point(1, 0)
      Point(-1, 0).moveReverse(West).moveReverse(West) shouldBe Point(1, 0)
      Point(0, 1).moveReverse(West) shouldBe Point(1, 1)
      Point(-1, 1).moveReverse(West).moveReverse(West) shouldBe Point(1, 1)
    }

    it("should move pieces SouthEast reverse correctly") {
      Point(0, 1).moveReverse(SouthEast) shouldBe Point(0, 0)
      Point(1, 2).moveReverse(SouthEast).moveReverse(SouthEast) shouldBe Point(0, 0)
      Point(1, 2).moveReverse(SouthEast) shouldBe Point(0, 1)
      Point(1, 3).moveReverse(SouthEast).moveReverse(SouthEast) shouldBe Point(0, 1)
    }

    it("should move pieces SouthWest reverse correctly") {
      Point(0, 1).moveReverse(SouthWest) shouldBe Point(1, 0)
      Point(0, 2).moveReverse(SouthWest).moveReverse(SouthWest) shouldBe Point(1, 0)
      Point(1, 2).moveReverse(SouthWest) shouldBe Point(1, 1)
      Point(0, 3).moveReverse(SouthWest).moveReverse(SouthWest) shouldBe Point(1, 1)
    }

    it("rotate big objects clockwise with an odd row pivot") {
      Point(1, 1).moveReverse(Clock, Point(1, 3)) shouldBe Point(0, 2)
      Point(2, 2).moveReverse(Clock, Point(1, 3)) shouldBe Point(1, 2)
      Point(4, 4).moveReverse(Clock, Point(1, 3)) shouldBe Point(3, 1)
    }

    it("rotate big objects counter clockwise with an odd row pivot") {
      Point(0, 4).moveReverse(CounterClock, Point(1, 3)) shouldBe Point(0, 2)
      Point(0, 3).moveReverse(CounterClock, Point(1, 3)) shouldBe Point(1, 2)
      Point(1, 0).moveReverse(CounterClock, Point(1, 3)) shouldBe Point(3, 1)
    }
  }

  describe("permutations") {
    it("should generate permutations of the one piece unit in a 2 x 2 board") {
      val block = Block(Set(Point(0, 0)), Point(0, 0))
      block.permutations(2, 2).map(_.members).toList should contain theSameElementsAs Seq(
        Set(Point(0, 0)),
        Set(Point(0, 1)),
        Set(Point(1, 0)),
        Set(Point(1, 1))
      )
    }

    it("should generate permutations of a simple two-piece unit in a 2 x 2 board") {
      val block = Block(Set(Point(0, 0), Point(1, 0)), Point(0, 0))
      block.permutations(2, 2).map(_.members).toList should contain theSameElementsAs Seq(
        Set(Point(1,0), Point(1,1)),
        Set(Point(0,1), Point(0,0)),
        Set(Point(0,1), Point(1,0)),
        Set(Point(0,1), Point(1,1)),
        Set(Point(1,0), Point(0,0))
      )
    }

    it("should generate permutations of a simple two-piece unit in a 3 x 3 board") {
      val block = Block(Set(Point(0, 0), Point(2, 0)), Point(1, 0))
      block.permutations(3, 3).map(_.members).toList should contain theSameElementsAs Seq(
        Set(Point(0,0), Point(2,0)),
        Set(Point(0,1), Point(2,1)),
        Set(Point(0,2), Point(2,2)),

        Set(Point(0,2), Point(1,0)),
        Set(Point(1,2), Point(2,0)),

        Set(Point(0,0), Point(1,2)),
        Set(Point(1,0), Point(2,2))
      )
    }

    it("l shape") {
      val block = Block(Set(Point(0,1),Point(0,2),Point(0,3)), Point(1,2))
      block.permutations(2, 4).map(_.members).toList should contain theSameElementsAs Seq(
        Set(Point(0,1),Point(0,2),Point(0,3)),
        Set(Point(1,0),Point(0,1),Point(1,2)),
        Set(Point(1,1),Point(1,2),Point(1,3)),

        Set(Point(0,0),Point(0,1),Point(1,1)),
        Set(Point(0,2),Point(0,3),Point(1,3)),

        Set(Point(1,1),Point(0,2),Point(1,2)),

        Set(Point(0,0),Point(0,1),Point(0,2)),
        Set(Point(1,0),Point(1,1),Point(1,2)),
        Set(Point(0,1),Point(1,2),Point(0,3)),

        Set(Point(0,0),Point(1,0),Point(1,1)),
        Set(Point(0,2),Point(1,2),Point(1,3)),

        Set(Point(0,1),Point(1,1),Point(0,2))
      )
    }

    it("squiggle") {
      val block = Block(Set(Point(0,1),Point(0,2),Point(0,3), Point(0,4)), Point(0,1))
      block.permutations(2, 5).map(_.members).toList should contain theSameElementsAs Seq(
        Set(Point(0,1),Point(0,2),Point(0,3),Point(0,4)),
        Set(Point(1,1),Point(1,2),Point(1,3),Point(1,4)),
        Set(Point(1,0),Point(0,1),Point(1,2),Point(0,3))
      )
    }
  }
}

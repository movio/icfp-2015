import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class FitnessTests extends FunSpec with ShouldMatchers {
  describe("AggregateDepthFitness") {
    describe("gives the sum of the depth") {
      it("counts the max depth for each column when there is nothing on board") {
        val board = Array(Array(false, false, false))
        val f = new AggergateDepthFitness(1)
        f.apply(board) shouldBe 3d
      }

      it("counts the depth as the first true it find in board for a column") {
        val board = Array(Array(true, false, false))
        val f = new AggergateDepthFitness(1)
        f.apply(board) shouldBe 0d
      }

      it("counts the max depth as the bottom if that is firt true") {
        val board = Array(Array(false, false, true))
        val f = new AggergateDepthFitness(1)
        f.apply(board) shouldBe 2d
      }

      it("counts the adds the depth for each column") {
        val board = Array(
          Array(false, false, false),
          Array(false, false, true),
          Array(false, true, false),
          Array(true, false, false)
        )
        val f = new AggergateDepthFitness(1)
        f.apply(board) shouldBe 6d
      }

      it("applies it's weight to the output") {
        val board = Array(
          Array(false, false, false),
          Array(false, false, true),
          Array(false, true, false),
          Array(true, false, false)
        )
        val f = new AggergateDepthFitness(0.5)
        f.apply(board) shouldBe 3d
      }
    }
  }

  describe("CompleteLinesFitness") {
    describe("gives the number of rows that will be cleared") {
      it("counts complete rows on an empty board as no cleard rows") {
        val board = Array(Array(false, false, false))
        val f = new CompleteLinesFitness(1)
        f.apply(board) shouldBe 0d
      }

      it("counts as one if there is only true values on first row") {
        val board = Array(Array(true, false, false))
        val f = new CompleteLinesFitness(1)
        f.apply(board) shouldBe 1d
      }

      it("counts as one if there is only true values on last row") {
        val board = Array(Array(false, false, true))
        val f = new CompleteLinesFitness(1)
        f.apply(board) shouldBe 1d
      }

      it("counts as zero if there is false values on a row") {
        val board = Array(Array(false), Array(false), Array(true))
        val f = new CompleteLinesFitness(1)
        f.apply(board) shouldBe 0d
      }

      it("counts as one if there is only true values on a row") {
        val board = Array(Array(true), Array(true), Array(true))
        val f = new CompleteLinesFitness(1)
        f.apply(board) shouldBe 1d
      }

      it("adds the counts for several completed rows") {
        val board = Array(
          Array(true, false, true),
          Array(true, false, true),
          Array(true, false, true),
          Array(true, false, true)
        )
        val f = new CompleteLinesFitness(1)
        f.apply(board) shouldBe 2d
      }

      it("applies it's weight to the output") {
        val board = Array(
          Array(true, false, true),
          Array(true, false, true),
          Array(true, false, true),
          Array(true, false, true)
        )
        val f = new CompleteLinesFitness(0.5)
        f.apply(board) shouldBe 1d
      }
    }
  }

  describe("HoleFitness") {
    describe("gives the negated number of spaces covered by blocks") {
      it("counts empty boards as zero") {
        val board = Array(Array(false, false, false))
        val f = new HoleFitness(1)
        f.apply(board) shouldBe 0
      }

      it("counts filled boards as zero") {
        val board = Array(Array(true,true,true))
        val f = new HoleFitness(1)
        f.apply(board) shouldBe 0
      }

      it("does not count open space") {
        val board = Array(Array(false,false,true))
        val f = new HoleFitness(1)
        f.apply(board) shouldBe 0
      }

      it("counts enclosed spaces") {
        val board = Array(Array(true,false,true))
        val f = new HoleFitness(1)
        f.apply(board) shouldBe -1d
      }

      it("counts each enclosed space") {
        val board = Array(Array(true,false,false))
        val f = new HoleFitness(1)
        f.apply(board) shouldBe -2d
      }

      it("sums the counts of enclosed spaces in each column") {
        val board = Array(
          Array(true,false,false),
          Array(false,true,false),
          Array(false,false,true)
        )
        val f = new HoleFitness(1)
        f.apply(board) shouldBe -3d
      }

      it("applies the weight") {
        val board = Array(
          Array(true,false,false),
          Array(false,true,false),
          Array(false,false,true)
        )
        val f = new HoleFitness(0.5)
        f.apply(board) shouldBe -1.5d
      }
    }
  }

  describe("BumpinessFitness") {
    describe("gives the negative sum of difference between column heights") {
      it("counts a single column as zero") {
        val board1 = Array(Array(false, false, false))
        val board2 = Array(Array(true, true, true))
        val f = new BumpinessFintness(1)
        f.apply(board1) shouldBe 0
        f.apply(board2) shouldBe 0
      }

      it("counts difference between adjacent columns") {
        val board1 = Array(
          Array(false, false, false),
          Array(false, false, true)
        )
        val f = new BumpinessFintness(1)
        f.apply(board1) shouldBe -1

        val board2 = Array(
          Array(false, false, true),
          Array(false, false, false)
        )
        f.apply(board2) shouldBe -1
      }

      it("counts difference between adjacent columns if the difference is bigger") {
        val board = Array(
          Array(true, true, false),
          Array(false, false, false)
        )
        val f = new BumpinessFintness(1)
        f.apply(board) shouldBe -3
      }

      it("adds up the differences") {
        val board = Array(
          Array(true, true, false),
          Array(false, false, false),
          Array(true, true, false)
        )
        val f = new BumpinessFintness(1)
        f.apply(board) shouldBe -6
      }

      it("applies the weight") {
        val board = Array(
          Array(true, true, false),
          Array(false, false, false),
          Array(true, true, false)
        )
        val f = new BumpinessFintness(3.0)
        f.apply(board) shouldBe -18d
      }
    }
  }

  describe("LineFullnessFitness") {
    describe("gives points for the number of filled cells in a row") {
      it("counts the true values on a single row") {
        val board = Array(Array(false), Array(false), Array(true))
        val f = new LineFullnessFitness(1)
        f.apply(board) shouldBe 1d
      }

      it("counts the true values on a single row if there is more than one") {
        val board1 = Array(Array(true), Array(false), Array(true))
        val f = new LineFullnessFitness(1)
        f.apply(board1) shouldBe 2d

        val board2 = Array(Array(false), Array(true), Array(true))
        f.apply(board2) shouldBe 2d
      }

      it("adds up the fullness of each row") {
        val board1 = Array(
          Array(true, true),
          Array(false, true),
          Array(true, false)
        )

        val f = new LineFullnessFitness(1)
        f.apply(board1) shouldBe 4d
      }

      it("applies the weight") {
        val board1 = Array(
          Array(true, true),
          Array(false, true),
          Array(true, false)
        )

        val f = new LineFullnessFitness(0.25)
        f.apply(board1) shouldBe 1d
      }

    }
  }
}

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
}

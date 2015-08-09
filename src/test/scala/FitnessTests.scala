
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class FitnessTests extends FunSpec with ShouldMatchers {
  describe("AggregateDepthFitness") {
    describe("gives the sum of the depth") {
      val f = new AggregateDepthFitness(1)
      it("counts the max depth for each column when there is nothing on board") {
        val board = Array(Array(false, false, false))
        f.apply(board) shouldBe 1d
      }

      it("counts the depth as the first true it find in board for a column") {
        val board = Array(Array(true, false, false))
        f.apply(board) shouldBe 0d
      }

      it("counts the max depth as the bottom if that is firt true") {
        val board = Array(Array(false, false, true))
        f.apply(board) shouldBe 2d / 3d
      }

      it("counts the adds the depth for each column") {
        val board = Array(
          Array(false, false, false),          
          Array(false, false, true),          
          Array(false, true, false),          
          Array(true, false, false)
        )
        f.apply(board) shouldBe 6d / (3 * 4)
      }

      it("applies it's weight to the output") {
        val f = new AggregateDepthFitness(0.5)
        val board = Array(
          Array(false, false, false),          
          Array(false, false, true),          
          Array(false, true, false),          
          Array(true, false, false)
        )
        f.apply(board) shouldBe 3d /(3 * 4)
      }
    }
  }

  describe("CompleteLinesFitness") {
    describe("gives the number of rows that will be cleared") {
      val f = new CompleteLinesFitness(1)
      it("counts complete rows on an empty board as no cleard rows") {
        val board = Array(Array(false, false, false))
        f.apply(board) shouldBe 0d
      }

      it("counts as one if there is only true values on first row") {
        val board = Array(Array(true, false, false))
        f.apply(board) shouldBe 1d / 3
      }

      it("counts as one if there is only true values on last row") {
        val board = Array(Array(false, false, true))
        f.apply(board) shouldBe 1d / 3
      }

      it("counts as zero if there is false values on a row") {
        val board = Array(Array(false), Array(false), Array(true))
        f.apply(board) shouldBe 0d
      }

      it("counts as one if there is only true values on a row") {
        val board = Array(Array(true), Array(true), Array(true))
        f.apply(board) shouldBe 1d
      }

      it("adds the counts for several completed rows") {
        val board = Array(
          Array(true, false, true),          
          Array(true, false, true),          
          Array(true, false, true),          
          Array(true, false, true)
        )
        f.apply(board) shouldBe 2d / 3
      }

      it("applies it's weight to the output") {
        val f = new CompleteLinesFitness(0.5)
        val board = Array(
          Array(true, false, true),          
          Array(true, false, true),          
          Array(true, false, true),          
          Array(true, false, true)
        )
        f.apply(board) shouldBe 1d / 3
      }
    }
  }

  describe("HoleFitness") {
    describe("gives the negated number of spaces covered by blocks") {
      val f = new HoleFitness(1)
      it("counts empty boards as zero") {
        val board = Array(Array(false, false, false))
        f.apply(board) shouldBe 0
      }

      it("counts filled boards as zero") {
        val board = Array(Array(true,true,true))
        f.apply(board) shouldBe 0
      }

      it("does not count open space") {
        val board = Array(Array(false,false,true))
        f.apply(board) shouldBe 0
      }

      it("counts enclosed spaces") {
        val board = Array(Array(true,false,true))
        f.apply(board) shouldBe -1d / 2
      }

      it("counts each enclosed space") {
        val board = Array(Array(true,false,false))
        f.apply(board) shouldBe -2d / 2
      }

      it("sums the counts of enclosed spaces in each column") {
        val board = Array(
          Array(true,false,false),          
          Array(false,true,false),          
          Array(false,false,true)
        )
        f.apply(board) shouldBe -3d / (2d*3)
      }

      it("applies the weight") {
        val f = new HoleFitness(0.5)
        val board = Array(
          Array(true,false,false),          
          Array(false,true,false),          
          Array(false,false,true)
        )
        f.apply(board) shouldBe -1.5d / (2d*3)
      }
    }
  }

  describe("BumpinessFitness") {
    describe("gives the negative sum of difference between column heights") {
      val f = new BumpinessFitness(1)

      it("counts a single column as zero") {
        val board1 = Array(Array(false, false, false))
        val board2 = Array(Array(true, true, true))
        f.apply(board1) shouldBe 0
        f.apply(board2) shouldBe 0
      }

      it("counts difference between adjacent columns") {
        val board1 = Array(
          Array(false, false, false),          
          Array(false, false, true)
        )
        f.apply(board1) shouldBe -1 / 3d

        val board2 = Array(
          Array(false, false, true),          
          Array(false, false, false)
        )
        f.apply(board2) shouldBe -1 / 3d
      }

      it("counts difference between adjacent columns if the difference is bigger") {
        val board = Array(
          Array(true, true, false),          
          Array(false, false, false)
        )
        f.apply(board) shouldBe -3 / 3d
      }

      it("adds up the differences") {

        val board = Array(
          Array(true, true, false),          
          Array(false, false, false),          
          Array(true, true, false)
        )
        f.apply(board) shouldBe -6 / 6d
      }

      it("applies the weight") {
        val f = new BumpinessFitness(3.0)
        val board = Array(
          Array(true, true, false),          
          Array(false, false, false),          
          Array(true, true, false)
        )
        f.apply(board) shouldBe -18d / 6d
      }
    }
  }

  describe("LineFullnessFitness") {
    describe("gives points for the number consecutively filled cells in a row") {
      val f = new LineFullnessFitness(1)
      it("counts the single true values on a row as zero") {

        val board = Array(Array(false), Array(false), Array(true))
        f.apply(board) shouldBe 0d
      }

      it("counts the true values on a single row if there is more than one") {
        val board1 = Array(Array(false), Array(true), Array(true))
        f.apply(board1) shouldBe 1d

        val board2 = Array(Array(true), Array(true), Array(true))
        f.apply(board2) shouldBe 4d


        val board = Array(Array(true), Array(true), Array(true), Array(true), Array(true), Array(true))
        f.apply(board) shouldBe 25d
      }

      it("counts each segment in in a broken up row") {
        val board = Array(Array(true), Array(true), Array(false), Array(true), Array(false), Array(true), Array(true))
        f.apply(board) shouldBe 4d
      }

      it("adds up the fullness of each row") {
        val board1 = Array(
          Array(true, true),          
          Array(true, false),          
          Array(false, true),          
          Array(true, true)
        )
        f.apply(board1) shouldBe 2d
      }

      it("applies the weight") {
        val f = new LineFullnessFitness(0.5)
        val board1 = Array(
          Array(true, true),          
          Array(false, true),          
          Array(true, false)
        )
        f.apply(board1) shouldBe 0.5d      
      }

    }
  }
}
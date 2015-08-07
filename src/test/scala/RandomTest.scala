import org.scalatest._

class RandomTest extends FunSpec with ShouldMatchers {
  it("first 10 values of seed 17 should be 0,24107,16552,12125,9427,13152,21440,3383,6873,16117") {
    val r = new Random(17)
    val xs = Stream.continually(r.next).take(10).toList
    xs should contain theSameElementsInOrderAs Seq(0,24107,16552,12125,9427,13152,21440,3383,6873,16117)
  }
}

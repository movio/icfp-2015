import java.io._
import collection.mutable
import spray.json._

object Point {
  import DefaultJsonProtocol._
  implicit val jf1 = jsonFormat2(Point.apply)
}
case class Point(
  x: Int,
  y: Int)

object Block {
  import DefaultJsonProtocol._
  implicit val jf2 = jsonFormat2(Block.apply)
}
case class Block(
  members: Set[Point],
  pivot: Point) {

  def move(move: Move): Block = {
    move match {
      case East ⇒ east()
      case West ⇒ west()
    }
  }

  private def east(): Block = {
    val newMembers = members map (p ⇒ p.copy(x = p.x + 1))
    val newPivot = pivot.copy(x = pivot.x + 1)
    Block(newMembers, newPivot)
  }

  private def west(): Block = {
    val newMembers = members map (p ⇒ p.copy(x = p.x - 1))
    val newPivot = pivot.copy(x = pivot.x - 1)
    Block(newMembers, newPivot)
  }

}

object Problem {
  import DefaultJsonProtocol._
  implicit val jf3 = jsonFormat(
    Problem.apply,
    "id",
    "units",
    "width",
    "height",
    "filled",
    "sourceLength",
    "sourceSeeds")
}
case class Problem(
  id: Int,
  units: Array[Block],
  width: Int,
  height: Int,
  filled: Set[Point],
  sourceLength: Int,
  sourceSeeds: Array[Int]) {

  val sources = mutable.Map.empty[Int, Source]

  def getSource(index: Int): Source = {
    sources.getOrElseUpdate(index, new Source(units, sourceSeeds(index), sourceLength))
  }
}

case class Source(units: Array[Block], seed: Int, length: Int) {
  val rand = new Random(seed)
  var count = 0

  def next(): Block = {
    count += 1
    if (count <= length)
      units(rand.next % units.length)
    else
      null
  }
}

object Problems {
  def load(i: Int): Problem = {
    var s: InputStream = null
    var r: BufferedReader = null
    val sb = new StringBuilder()

    try {
      s = getClass.getResourceAsStream(s"problem_$i.json")
      r = new BufferedReader(new InputStreamReader(s))

      var next = r.readLine()
      while (next != null) {
        sb.append(next)
        sb.append('\n')
        next = r.readLine()
      }
    } finally {
      try {
        if (r != null) r.close()
        if (s != null) s.close()
      } catch {
        case _: Throwable ⇒
      }
    }

    sb.toString.parseJson.convertTo[Problem]
  }
}

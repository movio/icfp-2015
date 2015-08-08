import java.io._
import collection.mutable
import spray.json._

object Point {
  import DefaultJsonProtocol._
  implicit val jf1 = jsonFormat2(Point.apply)
}
case class Point(
    x: Int,
    y: Int) {
  def move(move: Move, pivot: Point = Point(0,0)): Point = {
    move match {
      case East ⇒ east()
      case West ⇒ west()
      case SouthEast ⇒
        if (y % 2 == 0) // even
          south()
        else // odd
          south().move(East, pivot)
      case SouthWest ⇒
        if (y % 2 == 0) // even
          south().move(West, pivot)
        else //odd
          south()
      case Clock ⇒
        translate(pivot).toCubePoint().move(Clock).toPoint().untranslate(pivot)
      case CounterClock ⇒
        translate(pivot).toCubePoint().move(CounterClock).toPoint().untranslate(pivot)
    }
  }

  def translate(pivot: Point): Point =
    toCubePoint().translate(pivot.toCubePoint()).toPoint()

  def untranslate(pivot: Point): Point =
    toCubePoint().untranslate(pivot.toCubePoint()).toPoint()

  def toCubePoint(): CubePoint = {
    val cubeX = x - (y - (y & 1)) / 2
    val cubeZ = y
    val cubeY = -cubeX - cubeZ
    CubePoint(cubeX, cubeY, cubeZ)
  }

  private def east(): Point = copy(x = this.x + 1)
  private def west(): Point = copy(x = this.x - 1)
  private def south(): Point = copy(y = this.y + 1)
}

case class CubePoint(x: Int, y: Int, z: Int) {

  def toPoint(): Point = {
    val col = x + (z - (z & 1)) / 2
    val row = z
    Point(col, row)
  }

  def move(move: Move): CubePoint = {
    move match {
      case Clock        ⇒ CubePoint(-z, -x, -y)
      case CounterClock ⇒ CubePoint(-y, -z, -x)
    }
  }

  def translate(pivot: CubePoint): CubePoint =
    CubePoint(x - pivot.x, y - pivot.y, z - pivot.z)

  def untranslate(pivot: CubePoint): CubePoint =
    CubePoint(x + pivot.x, y + pivot.y, z + pivot.z)

}

object Block {
  import DefaultJsonProtocol._
  implicit val jf2 = jsonFormat2(Block.apply)
}
case class Block(
    members: Set[Point],
    pivot: Point) {

  def move(move: Move): Block =
    Block(members map (p ⇒ p.move(move, pivot)), pivot.move(move, pivot))

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

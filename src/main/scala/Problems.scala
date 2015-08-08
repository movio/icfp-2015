import java.io._
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
      case rotate: Rotate ⇒
        translate(pivot).toCubePoint().move(rotate).toPoint().untranslate(pivot)
    }
  }

  def moveReverse(move: Move, pivot: Point = Point(0,0)): Point = {
    move match {
      case East ⇒ west()
      case West ⇒ east()
      case SouthEast ⇒
        if (y % 2 == 0) // even
          north().move(West, pivot)
        else // odd
          north()
      case SouthWest ⇒
        if (y % 2 == 0) // even
          north()
        else //odd
          north().move(East, pivot)
      case Clock ⇒
        translate(pivot).toCubePoint().move(CounterClock).toPoint().untranslate(pivot)
      case CounterClock ⇒
        translate(pivot).toCubePoint().move(Clock).toPoint().untranslate(pivot)
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
  private def north(): Point = copy(y = this.y - 1)
}

case class CubePoint(x: Int, y: Int, z: Int) {

  def toPoint(): Point = {
    val col = x + (z - (z & 1)) / 2
    val row = z
    Point(col, row)
  }

  def move(move: Rotate): CubePoint = {
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

  def move(move: Move, times: Int = 1): Block =
    if (times <= 0)
      this
    else
      Block(members map (p ⇒ p.move(move, pivot)), pivot.move(move, pivot)).move(move, times - 1)

  def isInsideBoard(boardWidth: Int, boardHeight: Int): Boolean =
    members.forall(p => p.x < boardWidth && p.x >= 0 && p.y < boardHeight && p.y >= 0)

  def permutations(boardWidth: Int, boardHeight: Int): Set[Block] = {
    val blocks: IndexedSeq[Block] = for {
      x <- -boardWidth until boardWidth
      y <- -boardHeight until boardHeight
      r <- 0 to 5
    } yield copy().addX(x).addY(y).move(Clock, r)
    val setOfBlocks: Set[Block] = blocks.filter(_.isInsideBoard(boardWidth, boardHeight)).toSet
    setOfBlocks.groupBy(_.members).map(_._2.head).toSet // ignore pivot in comparison
  }

  def addX(delta: Int): Block =
    copy(members map (p => p.copy(x = p.x + delta)), pivot.copy(x = pivot.x + delta))

  def addY(delta: Int): Block =
    copy(members map (p => p.copy(y = p.y + delta)), pivot.copy(y = pivot.y + delta))

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

  def createSource(index: Int): Source = {
    new Source(units, sourceSeeds(index), sourceLength)
  }
}

case class Source(units: Array[Block], seed: Int, length: Int) {
  val rand = new Random(seed)
  var count = 0

  private def next(): Block = {
    count += 1
    if (count <= length)
      units(rand.next % units.length)
    else
      null
  }

  def asStream: Stream[Block] =
    Stream.continually(next()).takeWhile(_ != null)
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

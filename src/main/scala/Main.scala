import java.io._
import spray.json._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {

    @tailrec
    def findFiles(args: List[String], out: List[String]): List[String] =
      args match {
        case "-f" :: file :: rest ⇒ findFiles(rest, file :: out)
        case _ :: rest            ⇒ findFiles(rest, out)
        case Nil                  ⇒ out
      }

    val files = findFiles(args.toList, Nil)

    val solutions =
      files flatMap { path ⇒
        var r: BufferedReader = null
        val sb = new StringBuilder()

        try {
          r = new BufferedReader(new FileReader(path))

          var next = r.readLine()
          while (next != null) {
            sb.append(next)
            sb.append('\n')
            next = r.readLine()
          }
        } finally {
          try {
            if (r != null) r.close()
          } catch {
            case _: Throwable ⇒
          }
        }

        val problem = sb.toString.parseJson.convertTo[Problem]

        val solutions =
          (0 until problem.sourceSeeds.length) map { i ⇒
            val simulator = new Simulator(problem, i)
            simulator.createSolution()
          }

        solutions
      }

    import DefaultJsonProtocol._
    println(solutions.toJson.prettyPrint)
  }
}

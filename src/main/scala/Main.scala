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

    println(files)
  }
}

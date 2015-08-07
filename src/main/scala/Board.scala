object Pretty {
  def draw(map: Array[Array[Boolean]]): Unit = {
    val width = map.length
    val height = map(0).length

    def drawEvenRow(y: Int): Unit = {
      print("/")
      print(" V" * (width - 1))
      if (y == 0) print(" \\") else print(" V")
      println()

      for (x ← 0 until width) {
        print("|" + (if (map(x)(y)) "#" else " "))
      }
      println("|")
    }

    def drawOddRow(y: Int): Unit = {
      print(" V" * width)
      println(" \\")

      print(" ")
      for (x ← 0 until width) {
        print("|" + (if (map(x)(y)) "#" else " "))
      }
      println("|")
    }

    for (y ← 0 until height) {
      if (y % 2 == 0) drawEvenRow(y)
      else drawOddRow(y)
    }
    if (height % 2 == 0) print(" ")
    println(" V" * width)
  }
}

package com.github.akara.playground

object StringPathInMatrix extends App {

  val matrix = Vector(
    Vector('A', 'B', 'C', 'E'),
    Vector('S', 'F', 'C', 'S'),
    Vector('A', 'D', 'E', 'E')
  )

  //  var scanned = List.empty[(Int, Int)]

  def stringPathInMatrix(s: String): Option[List[(Int, Int)]] = {

    def matches(pos: (Int, Int), c: Char, scanned: List[(Int, Int)]) =
      !scanned.contains(pos) && matrix(pos._1)(pos._2) == c

    def find(s: String, scanned: List[(Int, Int)]): Option[List[(Int, Int)]] = {

      if (s.isEmpty) return Some(scanned)

      val currentPos = scanned.head
      val c = s.head
      var checkPos = currentPos.copy(_2 = currentPos._2 - 1) // left
      if (checkPos._2 >= 0 && matches(checkPos, c, scanned)) {
        println(s"Found $checkPos")
        val result = find(s.tail, checkPos :: scanned)
        if (result.isDefined) return result
      }
      checkPos = currentPos.copy(_1 = currentPos._1 - 1) // up
      if (checkPos._1 >= 0 && matches(checkPos, c, scanned)) {
        println(s"Found $checkPos")

        val result = find(s.tail, checkPos :: scanned)
        if (result.isDefined) return result
      }
      checkPos = currentPos.copy(_2 = currentPos._2 + 1) // right
      if (checkPos._2 < matrix(checkPos._1).size && matches(checkPos, c, scanned)) {
        println(s"Found $checkPos")
        val result = find(s.tail, checkPos :: scanned)
        if (result.isDefined) return result
      }
      checkPos = currentPos.copy(_1 = currentPos._1 + 1) // down
      println(s"Found $checkPos")
      if (checkPos._1 < matrix.size && matches(checkPos, c, scanned)) {
        val result = find(s.tail, checkPos :: scanned)
        if (result.isDefined) return result
      }
      None
    }

    def scan(str: String): Option[List[(Int, Int)]] = {
      for {
        i <- matrix.indices
        j <- matrix(i).indices
      } {
        if (str.head == matrix(i)(j)) {
          println(s"Found ($i,$j)")
          return find(str.tail, (i, j) :: Nil)
        }
      }
      None
    }

    scan(s).map(_.reverse)
  }

  val s = "BCCED"

  stringPathInMatrix(s) match {
    case Some(list) => println(s"Found $s: $list")
    case None => println("Not found")
  }
}

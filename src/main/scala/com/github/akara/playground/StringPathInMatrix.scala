package com.github.akara.playground

object StringPathInMatrix extends App {

  val matrix = Vector(
    Vector('A', 'B', 'C', 'E'),
    Vector('S', 'F', 'C', 'S'),
    Vector('A', 'D', 'E', 'E')
  )

  var scanned = List.empty[(Int, Int)]

  def stringPathInMatrix(s: String): Boolean = {

    def matches(pos: (Int, Int), c: Char) = !scanned.contains(pos) && matrix(pos._1)(pos._2) == c

    def found(c: Char): Boolean = {

      val currentPos = scanned.head

      var checkPos = currentPos.copy(_2 = currentPos._2 - 1) // left
      if (checkPos._2 >= 0 && matches(checkPos, c)) {
        scanned = checkPos :: scanned
        return true
      }
      checkPos = currentPos.copy(_1 = currentPos._1 - 1) // up
      if (checkPos._1 >= 0 && matches(checkPos, c)) {
        scanned = checkPos :: scanned
        return true
      }
      checkPos = currentPos.copy(_2 = currentPos._2 + 1) // right
      if (checkPos._2 < matrix(checkPos._1).size && matches(checkPos, c)) {
        scanned = checkPos :: scanned
        return true
      }
      checkPos = currentPos.copy(_1 = currentPos._1 + 1) // down
      if (checkPos._1 < matrix.size && matches(checkPos, c)) {
        scanned = checkPos :: scanned
        return true
      }
      false
    }

    def scan(str: String): Boolean = {
      for {
        i <- matrix.indices
        j <- matrix(i).indices
      } {
        if (str.head == matrix(i)(j)) {
          scanned = (i, j) :: Nil

          if (str.tail.forall(found)) return true
        }
      }
      false
    }

    scan(s)

  }

  val s = "BCCED"
  if (stringPathInMatrix(s)) println(s"Found $s: ${scanned.reverse}")
  else println("Not found")
}

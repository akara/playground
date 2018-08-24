package com.github.akara.playground

import scala.annotation.tailrec

object SumTwoElements extends App {

  def findElements(a: Array[Int], sum: Int): Option[(Int, Int)] = {

    @tailrec
    def check(i1: Int, i2: Int): Option[(Int, Int)] = {
      if (i1 >= i2 || i1 >= a.length || i2 <= 0) None
      else {
        val currentSum = a(i1) + a(i2)
        if (currentSum > sum) check(i1, i2 - 1)
        else if (currentSum < sum) check(i1 + 1, i2)
        else Some((a(i1), a(i2)))
      }
    }

    check (0, a.length - 1)
  }

  printf(findElements(Array(1, 2, 4, 7, 11, 15), 15).toString)

}

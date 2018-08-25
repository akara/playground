package com.github.akara.playground

import scala.annotation.tailrec
import scala.collection.mutable

object MaxInSlidingWindow extends App {

  val list = Seq(2, 3, 4, 2, 6, 2, 5, 1)

  val queue = mutable.Queue[Int]()

  @tailrec
  def maxInSlidingWindow(array: Seq[Int], windowSize: Int, currentResults: Seq[Int] = Seq.empty): Seq[Int] = {
    if (array.isEmpty) {
      currentResults :+ queue.max
    } else if (queue.size < windowSize) {
      queue += array.head
      maxInSlidingWindow(array.tail, windowSize, currentResults)
    } else {
      val max = queue.max
      queue.dequeue()
      queue += array.head
      maxInSlidingWindow(array.tail, windowSize, currentResults :+ max)
    }
  }

  println(maxInSlidingWindow(list, 3))
}

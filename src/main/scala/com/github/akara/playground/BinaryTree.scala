package com.github.akara.playground

import scala.collection.mutable.ArrayBuffer

object BinaryTree extends App {

  case class TreeNode[T](value: T, left: Option[TreeNode[T]] = None, right: Option[TreeNode[T]]= None)

  final def printTopToBottom[T](rootNode: TreeNode[T]): Unit = {
    val levelList = ArrayBuffer.empty[ArrayBuffer[T]]

    def collectLevel(value: T, level: Int): Unit = {
      if (levelList.size <= level) levelList += ArrayBuffer(value)
      else levelList(level) += value
    }

    def pushLevel(node: TreeNode[T], level: Int = 0): Unit = {
      collectLevel(node.value, level)
      node.left.foreach(pushLevel(_, level + 1))
      node.right.foreach(pushLevel(_, level + 1))
    }

    pushLevel(rootNode)
    levelList.foreach { level => println(level.mkString(", ")) }
  }

  def buildATree: TreeNode[Int] = {
    val n9 = TreeNode(9)
    val n8 = TreeNode(8)
    val n7 = TreeNode(7, right = Some(n9))
    val n6 = TreeNode(6, right = Some(n8))
    val n5 = TreeNode(5)
    val n4 = TreeNode(4)
    val n3 = TreeNode(3, Some(n6), Some(n7))
    val n2 = TreeNode(2, Some(n4), Some(n5))
    TreeNode(1, Some(n2), Some(n3))
  }

  printTopToBottom(buildATree)
}

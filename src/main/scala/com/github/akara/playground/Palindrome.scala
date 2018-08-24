package com.github.akara.playground

object Palindrome extends App {

  def palindrome(s: String): Int = {

    // 1. Each character itself is a palindrome
    val charCountP = s.length

    // 2. Each repeating character is a palindrome, with degree n
    var current = s.head
    var currentCount = 0
    var groupCountP = 0

    s.tail.foreach { c =>
      if (c == current) {
        currentCount += 1
      } else {
        groupCountP += currentCount
        current = c
        currentCount = 0
      }
    }

    // 3. Each substring of size n where n <= s.size / 2 can be a palindrome
    var tokenCountP = 0
    for {
      length <- 2 to (s.length / 2)
      index <- 0 to (s.length -  2 * length)
    } {
      val s1 = s.substring(index, index + length)
      val nextIdx = index + length
      val s2 = s.substring(nextIdx, nextIdx + length).reverse
      if (s1 == s2) tokenCountP += 1
    }

    // 4. Each substring of odd size n where n <= s.size / 2 + 1 can be a palindrome
    for {
      length <- 1 to (s.length / 2)
      index <- 0 to (s.length - (2 * length + 1))
    } {
      val s1 = s.substring(index, index + length)
      val nextIdx = index + length + 1
      val s2 = s.substring(nextIdx, nextIdx + length).reverse
      if (s1 == s2) tokenCountP += 1
    }

    charCountP + groupCountP + tokenCountP
  }

  def testPalindrome(s: String): Unit = {
    println(s"Palindrome for $s is ${palindrome(s)}")
  }

  testPalindrome("akka")
  testPalindrome("aka")
  testPalindrome("akkarrr")
  testPalindrome("abcba")

}


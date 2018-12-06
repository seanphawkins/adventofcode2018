package day5

import scala.annotation.tailrec
import scala.io.Source

object Main extends App {

  def shrink(s: String): String = {

    def isBalanced(s: String): Boolean = (s.length == 2) && (s(0) != s(1)) && (s.toUpperCase()(0) == s.toUpperCase()(1))

    @tailrec def s2(p: List[String], acc: String): String = p match {
      case Nil => acc
      case h +: Nil if isBalanced(h) => acc
      case h +: Nil => acc + h
      case h +: n +: Nil if isBalanced(h) => acc + n(1)
      case h +: _ +: t if isBalanced(h) => s2(t, acc)
      case h +: t => s2(t, acc + h(0))
    }

    @tailrec def shrinkString(s: String, ss: String): String = if (s == ss) s else shrinkString(ss, s2(ss.sliding(2, 1).toList, ""))

    shrinkString(s, s2(s.sliding(2, 1).toList, ""))
  }

  println(shrink(Source.fromResource("day5.txt").getLines().next()).length)
}

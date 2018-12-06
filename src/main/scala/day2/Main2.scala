package day2

import scala.io.Source

object Main2 extends App {

  def diffMagnitude(s1: String, s2: String): Int = s1.zip(s2).count(z => z._1 != z._2)
  def common(s1: String, s2: String): String = s1.zip(s2).filter(z => z._1 == z._2).map(_._1).mkString

  println(
    Source.fromResource("day2.txt").getLines.toList
      .combinations(2)
      .toList
      .filter(l => diffMagnitude(l.head, l.last) == 1)
      .map(l => common(l.head, l.last))
      .head
  )
}

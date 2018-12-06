package day2

import scala.io.Source

object Main extends App {

  def twoAndThree(s: String): (Int, Int) = {
    val g = s.groupBy(identity)
    (if (g.exists(_._2.length == 2)) 1 else 0, if (g.exists(_._2.length == 3)) 1 else 0)
  }
  def checksum(s: Iterator[String]): Int = {
    val p: (Int, Int) = s.map(twoAndThree).reduce[(Int, Int)]{ case (a: (Int, Int), b: (Int, Int)) => (a._1 + b._1, a._2 + b._2) }
    p._1 * p._2
  }

  println(checksum(Source.fromResource("day2.txt").getLines))
}

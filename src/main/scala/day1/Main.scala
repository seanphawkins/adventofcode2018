package day1

import scala.io.Source

object Main extends App {
  val v = Source.fromResource("day1.txt").getLines.map(_.toInt).sum
  println(v)
}
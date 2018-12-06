package day6

import scala.io.Source

object Main2 extends App {
  def distance(p1: (Int, Int), p2: (Int, Int)): Int = Math.abs(p1._1 - p2._1) + Math.abs(p1._2 - p2._2)

  val points = Source.fromResource("day6.txt").getLines().map(s => s.split(",").map(s => s.trim)).map(a => (a(0).toInt, a(1).toInt)).toList
  println((for (x <- 1 to points.map(_._1).max; y <- 1 to points.map(_._2).max) yield points.map(p => distance(p, (x, y))).sum).count(_ < 10000))
} 
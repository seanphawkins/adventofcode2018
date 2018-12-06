package day6

import scala.io.Source

object Main extends App {

  def distance(p1: (Int, Int), p2: (Int, Int)): Int = Math.abs(p1._1 - p2._1) + Math.abs(p1._2 - p2._2)

  def infiniteAreaPoints(p: List[(Int, Int)]): Set[(Int, Int)] = {
    val maxX = p.map(_._1).max
    val maxY = p.map(_._2).max
    val ts = (for (x <- 1 to maxX) yield closest((x, 1), p)).flatten.toSet
    val bs = (for (x <- 1 to maxX) yield closest((x, maxY), p)).flatten.toSet
    val ls = (for (y <- 1 to maxY) yield closest((1, y), p)).flatten.toSet
    val rs = (for (y <- 1 to maxY) yield closest((maxX, y), p)).flatten.toSet
    ts ++ bs ++ ls ++ rs
  }

  def closest(p: (Int, Int), l: List[(Int, Int)]): Option[(Int, Int)] = {
    val closestPoints = l.map(v => (v, distance(v, p))).groupBy(_._2).minBy(_._1)
    if (closestPoints._2.size > 1) None else Option(closestPoints._2.head._1)
  }

  val points = Source.fromResource("day6.txt").getLines().map(s => s.split(",").map(s => s.trim)).map(a => (a(0).toInt, a(1).toInt)).toList
  val c = (for (x <- 1 to points.map(_._1).max; y <- 1 to  points.map(_._2).max) yield closest((x, y), points)).flatten
  val ips = infiniteAreaPoints(points)
  println(c.filterNot(o => ips contains o).groupBy(identity).map(_._2.size).max)
}


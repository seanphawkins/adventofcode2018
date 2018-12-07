package day7

import scala.annotation.tailrec
import scala.io.Source

object Main extends App {

  val instructionRegex = raw"Step (.) must be finished before step (.) can begin\.".r

  def order(l: List[(String, String)]): String = {
    @tailrec def o(m: Map[String, Set[String]], acc: String): String = m.filter(_._2.isEmpty).toList.sortBy(_._1) match {
      case Nil => acc
      case h +: _ => o(m.mapValues(s => s - h._1) - h._1, acc + h._1)
    }
    val m = (Map.empty[String, Set[String]] /: l){ case (m, p) => m.updated(p._2, m.getOrElse(p._2, Set.empty) + p._1).updated(p._1, m.getOrElse(p._1, Set.empty)) }
    o(m, "")
  }

  println(order(Source.fromResource("day7.txt").getLines.collect { case instructionRegex(p, s) => (p, s) }.toList))
}

package day3

import scala.io.Source
import scala.util.matching.Regex

object Main extends App  {

  private val FabricSize = 1000

  val claims = Source.fromResource("day3.txt").getLines.map(Claim(_)).toArray
  val ss = for (x <- 0 to FabricSize; y <- 0 to FabricSize) yield if (claims.count(_.contains((x,y))) > 1) 1 else 0

  println(ss.sum)
}

final case class Claim(id: Int, upperLeft: (Int, Int), lowerRight: (Int, Int)) {
  def contains(p: (Int, Int)): Boolean = (p._1 >= upperLeft._1 && p._1 <= lowerRight._1) && (p._2 >= upperLeft._2 && p._2 <= lowerRight._2)
}
object Claim {
  def apply(s: String): Claim = {
    val claimPattern: Regex = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r
    s match {
      case claimPattern(id, topx, topy, deltax, deltay) => Claim(id.toInt, (topx.toInt, topy.toInt), (topx.toInt + deltax.toInt - 1, topy.toInt + deltay.toInt - 1))
      case _ => throw new RuntimeException("Could not parse claim string")
    }
  }
}

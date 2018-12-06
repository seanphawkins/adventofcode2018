package day3

import scala.io.Source

object Main2 extends App  {

  private val FabricSize = 1000

  val claims = Source.fromResource("day3.txt").getLines.map(Claim(_)).toList
  val ss = for (x <- 0 to FabricSize; y <- 0 to FabricSize) yield claims.filter(_.contains(x, y)).map(_.id)
  val idSet = ss.filter(_.size >1).flatten.toSet
  println((claims.map(_.id).toSet -- idSet).head)
}

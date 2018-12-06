package day1

import scala.annotation.tailrec
import scala.io.Source

object Main2 extends App {

  @tailrec def findDup(s: Iterator[Int], seen: Set[Int], currentVal: Int, refresh: => Iterator[Int]): Int = {
    if (s.isEmpty)
      findDup(refresh, seen, currentVal, refresh)
    else
      currentVal + s.next() match {
        case v if seen.contains(v) => v
        case v => findDup(s, seen + v, v, refresh)
      }
  }

  def getInts: Iterator[Int] = Source.fromResource("day1.txt").getLines.map(_.toInt)

  println(findDup(getInts, Set(0), 0, getInts))
}
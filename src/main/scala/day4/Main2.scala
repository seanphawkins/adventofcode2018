package day4

import java.time.{LocalDateTime, ZoneOffset}
import day4.Main.timeAsleep
import scala.io.Source

object Main2 extends App {
  implicit val localDateOrdering: Ordering[LocalDateTime] = Ordering.by(_.toInstant(ZoneOffset.UTC).toEpochMilli)
  val log = Source.fromResource("day4.txt").getLines.map(LogEvent(_)).toList.sortBy(_.timestamp)

  val sleepMap = timeAsleep(log, -1, Map.empty[Int, Array[Int]])
  val sleepiest  = sleepMap.map(g => g._2.toList.zipWithIndex.map(freq => (g._1, freq))).flatten
  val sleepyMinute = sleepiest.maxBy(_._2._1)
  println(sleepyMinute._1 * sleepyMinute._2._2)
}

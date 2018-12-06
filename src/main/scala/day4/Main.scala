package day4

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}

import scala.annotation.tailrec
import scala.io.Source

sealed trait LogEvent {
  def timestamp: LocalDateTime
}
object LogEvent {
  private val startPattern = raw"\[(\d\d\d\d-\d\d-\d\d \d\d:\d\d)\] Guard #(\d+) begins shift".r
  private val asleepPattern = raw"\[(\d\d\d\d-\d\d-\d\d \d\d:\d\d)\] falls asleep".r
  private val awakePattern = raw"\[(\d\d\d\d-\d\d-\d\d \d\d:\d\d)\] wakes up".r

  private val df = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def apply(s: String): LogEvent = s match {
    case startPattern(ts, id) => GuardStarts(id.toInt, LocalDateTime.parse(ts, df))
    case asleepPattern(ts) => FallsAsleep(LocalDateTime.parse(ts, df))
    case awakePattern(ts) => WakesUp(LocalDateTime.parse(ts, df))
    case _ => throw new RuntimeException("could not parse input")
  }
}

final case class GuardStarts(id: Int, timestamp: LocalDateTime) extends LogEvent
final case class FallsAsleep(timestamp: LocalDateTime) extends LogEvent
final case class WakesUp(timestamp: LocalDateTime) extends LogEvent

object Main extends App  {
  @tailrec def timeAsleep(l: Seq[LogEvent], currentGuard: Int, acc: Map[Int, Array[Int]]): Map[Int, Array[Int]] = l match {
    case Nil => acc
    case GuardStarts(n, _) +: t =>
      timeAsleep(t, n, acc)
    case FallsAsleep(t1) +: WakesUp(t2) +: t =>
      val sa = acc.getOrElse(currentGuard, Array.ofDim[Int](60))
      (t1.getMinute until t2.getMinute) foreach { i=> sa(i) = sa(i) + 1 }
      timeAsleep(t, currentGuard, acc.updated(currentGuard, sa))
  }

  implicit val localDateOrdering: Ordering[LocalDateTime] = Ordering.by(_.toInstant(ZoneOffset.UTC).toEpochMilli)
  val log = Source.fromResource("day4.txt").getLines.map(LogEvent(_)).toList.sortBy(_.timestamp)

  val sleepMap = timeAsleep(log, -1, Map.empty[Int, Array[Int]])
  val sleepiest = sleepMap.maxBy(_._2.sum)
  val sleepiestGuard = sleepiest._1
  val sleepiestMinute = sleepiest._2.zipWithIndex.maxBy(_._1)._2
  println(sleepiestGuard * sleepiestMinute)
}

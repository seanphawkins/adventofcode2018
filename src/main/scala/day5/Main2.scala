package day5

import day5.Main.shrink

import scala.io.Source

object Main2 extends App {
  val polymer = Source.fromResource("day5.txt").getLines().next()
  val letters = polymer.toUpperCase().toSet
  println(letters.map(c => shrink(polymer.filterNot(x => x == c || x == c.toLower)).length).min)
}
package day8

import scala.io.Source

@main def main(): Unit =
  val input = Source.fromResource("day8/input.txt").getLines.toList
  val width = input.head.length
  val height = input.size

  val inputMap = InputMap(input.mkString.toCharArray, width, height)

  println(s"Part 1 result: ${inputMap.antinodes.size}")
  println(s"Part 2 result: ${inputMap.antinodesWithHarmonics.size}")

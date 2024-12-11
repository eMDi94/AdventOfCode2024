package day10

import scala.io.Source

@main def main(): Unit =
  val input = Source.fromResource("day10/input.txt").getLines.toList
  val width = input.head.length
  val height = input.size
  val data = input.mkString.map(_.asDigit).toList.toVector

  val map = TopographicMap(data, width, height)
  
  // The result is all the distinct 9 that can be reached
  val part1Result = map.trailheads.flatMap(map.walk(_).distinct).size
  println(s"Part1 Result: $part1Result")
  
  // The result is all the 9 that can be reached. If the same 9 is repeated, it means that the same point is reachable
  // through different paths
  val part2Result = map.trailheads.flatMap(map.walk).size
  println(s"Part2 Result: $part2Result")
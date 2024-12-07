package day6

import scala.util.{Success, Failure}
import scala.io.Source

@main def part1Main(): Unit =
  val input = Source.fromResource("day6/input.txt").getLines.toList
  val width = input.head.length
  val height = input.size
  val fieldMap = FieldMap(input.mkString.toArray, width, height)

  fieldMap.walk match
    case Success(visitedPoints) =>
      println(s"The result is ${visitedPoints.map(_.point).size + 1}") // Because we have to include the last step to get outside of the map
    case Failure(e) =>
      println(e.getMessage)
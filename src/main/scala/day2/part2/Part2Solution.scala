package day2.part2

import scala.io.Source

object Part2Solution extends App:

  private val input = Source.fromResource("day2/input.txt")
    .getLines
    .map(ProblemDampenerLine.fromFileLine)

  private val solution = input.count(_.isSafe)

  println(s"The result is $solution")

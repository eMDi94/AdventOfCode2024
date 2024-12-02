package day2.part1

import scala.io.Source


object Part1Solution extends App:

  private val input = Source.fromResource("day2/input.txt")
    .getLines
    .map(Line.fromFileLine)

  private val result = input.count(_.isSafe)

  println(s"The result is $result")

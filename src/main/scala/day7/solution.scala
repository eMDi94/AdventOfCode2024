package day7

import scala.io.Source

@main def main(): Unit =
  val input = Source.fromResource("day7/input.txt").getLines
  val parsedInput = input.map(parseLine)

  val result = parsedInput
    .filter(_.nonEmpty)
    .map(_.get)
    .filter(_.isSolvable)
    .foldLeft(0L)((acc, equation) => acc + equation.result)

  println(s"The result is $result")

def parseLine(line: String): Option[Equation] =
  line.split(":") match
    case Array(result, numbers, _*) =>
      val parsedNumbers = numbers.split(" ").filter(_.nonEmpty).map(_.trim.toLong)
      Some(Equation(result.trim.toLong, parsedNumbers.toList))
    case _ => None
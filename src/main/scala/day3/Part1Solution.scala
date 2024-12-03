package day3

import scala.io.Source

object Part1Solution extends App:

  private val input = Source.fromResource("day3/input.txt").getLines.mkString("\n")

  private val mulRegex = "mul\\(\\d{1,3},\\d{1,3}\\)".r
  private val numberRegex = "\\d{1,3}".r

  private val values = mulRegex.findAllMatchIn(input)
    .flatMap(mulMatch => numberRegex.findAllMatchIn(mulMatch.toString).map(_.toString.toInt))
    .toList

  private val result = values.grouped(2)
    .map {
      case first :: second :: Nil => first * second
      case _ => 0
    }
    .sum

  println(s"The result is $result")

package day3

import scala.io.Source

object Part2Solution extends App:

  private case class Accumulator(enabled: Boolean = true, currentResult: Int = 0)

  private val input = Source.fromResource("day3/input.txt").getLines.mkString("\n")

  private val primaryRegex = "mul\\(\\d{1,3},\\d{1,3}\\)|don't\\(\\)|do\\(\\)".r
  private val numRegex = "\\d{1,3}".r

  private val result = primaryRegex.findAllMatchIn(input).foldLeft(Accumulator()){ (acc, current) =>
    (current.matched, acc.enabled) match {
      case ("don't()", _) => acc.copy(enabled = false)
      case ("do()", _) => acc.copy(enabled = true)
      case (matched, true) if matched.startsWith("mul") =>
        val product = extractNumbersAndMultiply(matched)
        acc.copy(currentResult = acc.currentResult + product)
      case _ => acc
    }
  }

  println(s"The result is ${result.currentResult}")

  private def extractNumbersAndMultiply(mul: String) =
    numRegex.findAllMatchIn(mul).map(_.matched.toInt).product

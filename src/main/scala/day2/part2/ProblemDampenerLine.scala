package day2.part2

import scala.annotation.tailrec

case class ProblemDampenerLine(line: List[Int]):

  def isSafe: Boolean = onOffElementInLine(List.empty, line)

  @tailrec
  private def onOffElementInLine(listHead: List[Int], listTail: List[Int]): Boolean =
    listTail match {
      case head :: tail =>
        val canBeSafe = computeLineIsSafe(listHead ++ listTail) || computeLineIsSafe(listHead ++ tail)
        canBeSafe || onOffElementInLine(listHead :+ head, tail)
      case Nil => false
    }

  private val computeLineIsSafe = computeDifferences andThen isDifferencesSafe

  private def computeDifferences(line: List[Int]) =
    line.tail.zip(line).map(_ - _)

  private def isDifferencesSafe(differences: List[Int]) =
    allIncreasingOrDecreasing(differences) && allWithinRange(differences)

  private def allIncreasingOrDecreasing(differences: List[Int]): Boolean =
    differences.forall(_ < 0) || differences.forall(_ > 0)

  private def allWithinRange(differences: List[Int]): Boolean =
    differences.forall(isInRange)

  private def isInRange(value: Int): Boolean =
    val absValue = Math.abs(value)
    absValue >= 1 && absValue <= 3


object ProblemDampenerLine:

  def fromFileLine(fileLine: String): ProblemDampenerLine =
    ProblemDampenerLine(fileLine.split("\\s").filter(_.nonEmpty).map(_.toInt).toList)

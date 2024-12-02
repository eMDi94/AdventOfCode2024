package day2.part1

case class Line(line: Seq[Int]):

  def isSafe: Boolean = allIncreasingOrDecreasing && allWithinRange

  private lazy val differences: Seq[Int] = line.tail.zip(line).map(_ - _)
    
  protected def getSign(value: Int): Int = Math.abs(value) / value
  
  private def allIncreasingOrDecreasing: Boolean = differences.forall(_ < 0) || differences.forall(_ > 0)
  private def allWithinRange: Boolean = differences.forall(isInRange)
  private def isInRange(value: Int): Boolean =
    val absValue = Math.abs(value)
    absValue >= 1 && absValue <= 3


object Line:

  def fromFileLine(fileLine: String): Line =
    Line(fileLine.split("\\s").filter(_.nonEmpty).map(_.toInt).toSeq)

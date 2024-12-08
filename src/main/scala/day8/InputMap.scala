package day8

import scala.annotation.tailrec
import scala.collection.immutable

case class InputMap(data: Array[Char], width: Int, height: Int):

  import InputMap._

  private val rowRange = 0 until height
  private val columnRange = 0 until width

  def antinodes: Set[Point] = groupedAntennas
    .flatMap(e => computeAntinodes(e._2.map(_._2).toList))
    .toSet

  def antinodesWithHarmonics: Set[Point] = groupedAntennas
    .flatMap(e => computeAntinodesWithHarmonics(e._2.map(_._2).toList))
    .toSet

  private def antennas = data.zipWithIndex
    .filter((elem, _) => elem.isLower || elem.isUpper || elem.isDigit)
    .map((elem, index) => (elem, getPointFromIndex(index)))

  private def getPointFromIndex(index: Int) =
    val row = index / width
    val column = index - (row * width)
    Point(row, column)

  private def groupedAntennas = antennas.groupBy(_._1)

  private def computeAntinodes(nodes: List[Point]) =
    nodes.combinations(2).flatMap { twoNodes =>
      twoNodes.permutations.flatMap {
        case p1 :: p2 :: Nil => antinode(p1, p2)
        case _ => List.empty
      }
    }

  private def computeAntinodesWithHarmonics(nodes: List[Point]) =
    nodes.combinations(2).flatMap { twoNodes =>
      twoNodes.permutations.flatMap {
        case p1 :: p2 :: Nil => antinodeWithHarmonic(p1, p2)
        case _ => List.empty
      }
    }

  private def antinode(p1: Point, p2: Point) =
    val newRow = p2.row + (p2.row - p1.row)
    val newColumn = p2.column + (p2.column - p1.column)
    (rowRange.contains(newRow), columnRange.contains(newColumn)) match
      case (true, true) => List(Point(newRow, newColumn))
      case _ => List.empty

  private def antinodeWithHarmonic(p1: Point, p2: Point) =
    val rowDisplacement = p2.row - p1.row
    val columnDisplacement = p2.column - p1.column
    val newRow = p2.row + rowDisplacement
    val newColumn = p2.column + columnDisplacement
    computeAntinodesOnLine(p2.copy(), rowDisplacement, columnDisplacement, List.empty)

  @tailrec
  private def computeAntinodesOnLine(currentPoint: Point, rowDisplacement: Int, columnDisplacement: Int, currentAntinodes: List[Point]): List[Point] =
    (rowRange.contains(currentPoint.row), columnRange.contains(currentPoint.column)) match
      case (true, true) =>
        val newPoint = Point(currentPoint.row + rowDisplacement, currentPoint.column + columnDisplacement)
        computeAntinodesOnLine(newPoint, rowDisplacement, columnDisplacement, currentAntinodes :+ currentPoint.copy())
      case _ =>
        currentAntinodes

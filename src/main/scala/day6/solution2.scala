package day6

import scala.util.{Success, Failure}
import scala.io.Source

case class MissingStartPoint(message: String, cause: Option[Throwable] = None) extends Throwable(message, cause.orNull)

@main def part2Main(): Unit =
  val input = Source.fromResource("day6/input.txt").getLines.toList
  val width = input.head.length
  val height = input.size
  val fieldMap = FieldMap(input.mkString.toArray, width, height)

  val dependencies = for
    visitedLocations <- fieldMap.walk
    startPoint <- findStartPoint(fieldMap)
  yield (visitedLocations, startPoint)

  dependencies match
    case Success((visitedLocations, startPoint)) =>
      val visitedPoints = visitedLocations.map(_.point).filter(_ != startPoint)
      val loops = visitedPoints.count { visitedPoint =>
        val newFieldMap = fieldMap.updateCell(visitedPoint, '#')
        newFieldMap.walk.isFailure
      }
      println(s"The result is $loops")
    case Failure(exception) =>
      println(exception.getMessage)


def findStartPoint(fieldMap: FieldMap) =
  Direction.values.collectFirst { v =>
    fieldMap.searchCharacter(v.idCharacter) match
      case Some(point) => point
  }.toRight(MissingStartPoint("No direction found in the starting grid")).toTry

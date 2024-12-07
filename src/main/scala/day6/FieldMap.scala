package day6

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class FieldMap(data: Array[Char], width: Int, height: Int):
  
  import FieldMap._
  
  private case class State(pointVector: PointVector, visitedPoints: Set[PointVector] = Set.empty)

  def apply(row: Int, column: Int): Char =
    data(row * width + column)

  def apply(point: Point): Char =
    this(point.x, point.y)
    
  def walk: Try[Set[PointVector]] =
    val start = Direction.values.collectFirst { currentDirection =>
      searchCharacter(currentDirection.idCharacter) match {
        case Some(mapFieldPoint) => PointVector(mapFieldPoint, currentDirection)
      }
    }

    start match
      case Some(pointVector) => nextStep(State(pointVector))
      case None => Failure(NoStartPoint("No start point found"))
      
  def updateCell(point: Point, updatedValue: Char): FieldMap =
    val newData = data.updated(point.x * width + point.y, updatedValue)
    copy(data = newData)

  def searchCharacter(char: Char): Option[Point] =
    data.zipWithIndex.collectFirst {
      case (currentCharacter, index) if currentCharacter == char =>
        val row = index / width
        val column = index - (row * width)
        Point(row, column)
    }

  private def inMap(point: Point): Boolean =
    point.x >= 0 && point.x < height && point.y >= 0 && point.y < width

  @tailrec
  private def nextStep(state: State): Try[Set[PointVector]] =
    if state.visitedPoints.contains(state.pointVector) then
      Failure(LoopException("Found loop"))
    else
      val lookaheadPoint = state.pointVector.point + state.pointVector.direction.displacement
      val newVisitedPoints = state.visitedPoints + state.pointVector.copy()
      if inMap(lookaheadPoint) then
        this(lookaheadPoint) match
          case '#' =>
            val nextDirection = state.pointVector.direction.next
            val nextPointVector = state.pointVector.copy(direction = nextDirection)
            nextStep(state.copy(pointVector = nextPointVector, visitedPoints = newVisitedPoints))
          case _ =>
            nextStep(state.copy(pointVector = state.pointVector.copy(point = lookaheadPoint), visitedPoints = newVisitedPoints))
      else
        Success(state.visitedPoints)
        


object FieldMap:
  
  case class PointVector(point: Point, direction: Direction)
  case class LoopException(message: String, cause: Option[Throwable] = None) extends Throwable(message, cause.orNull)
  case class NoStartPoint(message: String, cause: Option[Throwable] = None) extends Throwable(message, cause.orNull)

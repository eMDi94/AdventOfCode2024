package day20

import scala.annotation.tailrec

case class Point(row: Int, column: Int):

  def +(other: Point): Point = Point(row + other.row, column + other.column)
  def +(direction: Direction): Point = this + direction.displacement

  def neighbours: Seq[Point] = Direction.values.map(this + _).toVector
  
  def linearDistance(other: Point): Int =
    (row - other.row).abs + (column - other.column).abs

case class Dimension(height: Int, width: Int)

enum Direction(val displacement: Point):
  case Up extends Direction(Point(-1, 0))
  case East extends Direction(Point(0, 1))
  case South extends Direction(Point(1, 0))
  case West extends Direction(Point(0, -1))

case class Board(data: Vector[Char])(using dimension: Dimension):

  def apply(point: Point): Char = data(point.index)
  
  def get(point: Point): Option[Char] =
    if inBoard(point) then Some(this(point)) else None

  def minimumPath(start: Point, end: Point): Option[Vector[Point]] =
    @tailrec
    def recursiveHelper(priorityQueue: PriorityQueue[Int, Point], distances: Map[Point, Int], fromMap: Map[Point, Point]): Option[Map[Point, Point]] =
      if priorityQueue.isEmpty then None
      else
        val (priority, head, rest) = priorityQueue.dequeue
        head match
          case `end` => Some(fromMap)
          case currentPosition =>
            val newScore = priority + 1
            val (newQueue, newDistances, newFromToMap) = currentPosition.neighbours
              .filter(loc => inBoard(loc) && this(loc) != '#')
              .foldLeft((rest, distances, fromMap)) { (acc, neighbour) =>
                val (queue, distances, fromToMap) = acc
                if newScore < distances.getOrElse(neighbour, Int.MaxValue) then
                  (queue.enqueue(newScore, neighbour), distances + (neighbour -> newScore), fromToMap + (neighbour -> currentPosition))
                else
                  acc
              }
            recursiveHelper(newQueue, newDistances, newFromToMap)

    val priorityQueue = PriorityQueue(0, start)
    recursiveHelper(priorityQueue, Map.empty, Map.empty).map(reconstructPath(start, end))

  def findElement(element: Char): Option[Point] =
    data.zipWithIndex.collectFirst {
      case (char, index) if char == element => index.position
    }

  def findAll(element: Char): Seq[Point] =
    data.zipWithIndex.collect {
      case (char, index) if char == element => index.position
    }
    
  def updateBoard(point: Point, element: Char): Board =
    Board(data.updated(point.index, element))

  private def reconstructPath(start: Point, end: Point)(fromMap: Map[Point, Point]) =
    @tailrec
    def recursiveHelper(currentPosition: Point, path: Vector[Point] = Vector.empty): Vector[Point] =
      fromMap(currentPosition) match
        case `start` =>
          (path :+ start).reverse
        case nextPosition =>
          recursiveHelper(nextPosition, path :+ currentPosition)

    recursiveHelper(end)

  private def inBoard(point: Point) =
    point.row >= 0 && point.row < dimension.height && point.column >= 0 && point.column < dimension.width

  extension (self: Int)
    def position: Point =
      val row = self / dimension.width
      val column = self -  dimension.width * row
      Point(row, column)

  extension (self: Point)
    def index: Int = self.row * dimension.width + self.column

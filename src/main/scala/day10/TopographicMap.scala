package day10

import scala.annotation.tailrec

case class Point(row: Int, column: Int):

  def +(other: Point): Point = Point(row + other.row, column + other.column)

case class TopographicMap(data: Vector[Int], width: Int, height: Int):

  def apply(row: Int, column: Int): Int = data(row * width + column)
  def apply(point: Point): Int = this(point.row, point.column)
  
  def trailheads: Seq[Point] = 
    data.indices.filter(data(_) == 0).map(convertIndexToPoint)

  def walk(position: Point): Seq[Point] = this(position) match
    case 9 => Seq(position)
    case n => neighbours(position).filter(apply(_) == n + 1).flatMap(walk)

  private def convertIndexToPoint(index: Int) =
    val row = index / width
    val column = index - (row * width)
    Point(row, column)

  private def neighbours(point: Point) =
    Seq(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))
      .map(point.+)
      .filter {
        case Point(row, column) => row >= 0 && row < height && column >= 0 && column < width
      }

package day14

case class Board(height: Int, width: Int)

enum Quadrant:
  case I, II, III, IV

case class Point(row: Int, column: Int):

  def +(other: Point): Point = Point(row + other.row, column + other.column)
  def *(multiplier: Int): Point = Point(row * multiplier, column * multiplier)

case class Robot(currentPosition: Point, velocity: Point)(using board: Board):

  def at(time: Int): Robot =
    copy(velocity = velocity * time).move

  def move: Robot =
    val newRow = moveCoord(currentPosition.row, velocity.row, board.height)
    val newColumn = moveCoord(currentPosition.column, velocity.column, board.width)
    copy(currentPosition = Point(newRow, newColumn))

  def quadrant: Option[Quadrant] =
    ((currentPosition.row - board.height / 2).sign, (currentPosition.column - board.width / 2).sign) match
      case (-1, -1) => Some(Quadrant.I)
      case (-1, 1) => Some(Quadrant.II)
      case (1, -1) => Some(Quadrant.III)
      case (1, 1) => Some(Quadrant.IV)
      case _ => None

  private def moveCoord(coord: Int, displacement: Int, upperBound: Int) =
    val newCoord = (coord + displacement) % upperBound
    if newCoord < 0 then upperBound + newCoord else newCoord

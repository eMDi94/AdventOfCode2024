package day16

type Signum = -1 | 0 | 1

enum Direction(val row: Signum, val column: Signum):
  case North extends Direction(-1, 0)
  case East extends Direction(0, 1)
  case South extends Direction(1, 0)
  case West extends Direction(0, -1)

  def clockwise: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def counterClockwise: Direction = this match
    case Direction.North => Direction.West
    case Direction.East => Direction.North
    case Direction.South => Direction.East
    case Direction.West => Direction.South


case class Position(row: Int, column: Int):

  def +(other: Position): Position = Position(row + other.row, column + other.column)
  def +(direction: Direction): Position = Position(row + direction.row, column + direction.column)


case class Dimension(height: Int, width: Int)

case class Maze(data: Vector[Char])(using dimension: Dimension):

  def apply(position: Position): Char = data(position.index)

  def findPosition(char: Char): Option[Position] =
    data.zipWithIndex.collectFirst {
      case (c, index) if c == char => index.position
    }

  extension (self: Int)
    private def position: Position =
      val row = self / dimension.width
      val column = self - dimension.width * row
      Position(row, column)

  extension (self: Position)
    private def index: Int = self.row * dimension.width + self.column

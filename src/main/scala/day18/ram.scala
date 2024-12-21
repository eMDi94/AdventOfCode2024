package day18

case class Dimension(height: Int, width: Int)

case class Position(row: Int, column: Int):

  def neighbours: Seq[Position] =
    Seq(Position(row - 1, column), Position(row + 1, column), Position(row, column - 1), Position(row, column + 1))


class Ram(invalidLocations: Vector[Position])(using dimension: Dimension):

  def apply(position: Position): Char =
    invalidLocations.find(_ == position)
      .map(_ => '#')
      .getOrElse('.')

  def inRam(position: Position): Boolean =
    position.row >= 0 && position.row < dimension.height && position.column >= 0 && position.column < dimension.width

  extension (self: Int)
    def position: Position =
      val row = self / dimension.width
      val column = self - row * dimension.width
      Position(row, column)

  extension (self: Position)
    def index: Int = self.row * dimension.width + self.column

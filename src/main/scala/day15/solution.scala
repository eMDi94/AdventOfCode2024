package day15

import scala.annotation.tailrec
import scala.io.Source

@main def main(): Unit =
  val input = Source.fromResource("day15/input.txt").getLines.mkString("\n")
  val part2 = true
  val (board, commands) = input.split("\n\n") match
    case Array(data, commands) =>
      val lines = data.split("\n")

      val filteredData = data.toVector.filter(_ != '\n')
      val (boardData, boardDimensions) = if part2 then
        val dimension = Dimension(lines.size * 2, lines.head.length * 2)
        val data = filteredData
          .foldLeft(Vector.empty[Char]) {
            case (acc, BoardElements.WALL) => acc ++ Vector(BoardElements.WALL, BoardElements.WALL)
            case (acc, BoardElements.BOX) => acc ++ Vector(BoardElements.OPEN_BOX, BoardElements.CLOSE_BOX)
            case (acc, BoardElements.ROBOT) => acc ++ Vector(BoardElements.ROBOT, BoardElements.EMPTY)
            case (acc, BoardElements.EMPTY) => acc ++ Vector(BoardElements.EMPTY, BoardElements.EMPTY)
            case (acc, _) => acc
          }
        (data, dimension)
      else
        val dimension = Dimension(lines.size, lines.head.length)
        (filteredData, dimension)

      given Dimension = boardDimensions
      val board = Board(boardData)

      (board, commands.replace("\n", "").toList)

  applyCommands(Some(board), commands) match
    case Some(b) =>
      val result = b.findAll(if part2 then BoardElements.OPEN_BOX else BoardElements.BOX)
        .map(position => position.row * 100 + position.column).sum
      println(s"The result is $result")
    case None => println("Invalid board")



@tailrec
def applyCommands(board: Option[Board], commands: List[Char]): Option[Board] =
  (board, commands) match
    case (Some(board), commandChar :: tail) =>
      val command = Command.fromCharValue(commandChar)
      val newBoard = command.flatMap(board.move)
      applyCommands(newBoard, tail)
    case (board, Nil) => board
    case (None, _) => None

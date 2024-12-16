package day15

case class Dimension(height: Int, width: Int)

case class Position(row: Int, column: Int):

  def +(other: Position): Position = Position(row +  other.row, column + other.column)
  def +(command: Command): Position = this + command.displacement

enum Command(val identifier: Char, val displacement: Position):
  case Up extends Command('^', Position(-1, 0))
  case Right extends Command('>', Position(0, 1))
  case Down extends Command('v', Position(1, 0))
  case Left extends Command('<', Position(0, -1))


object Command:

  def fromCharValue(char: Char): Option[Command] =
    Command.values.collectFirst {
      case command if command.identifier == char => command
    }


object BoardElements:
  val WALL = '#'
  val ROBOT = '@'
  val BOX = 'O'
  val OPEN_BOX = '['
  val CLOSE_BOX = ']'
  val EMPTY = '.'

type BoardElement = Char

case class Board(data: Vector[Char])(using dimension: Dimension):

  def apply(position: Position): Char = data(position.index)

  def move(command: Command): Option[Board] = robot.map(moveElement(_, command).getOrElse(this))
  
  /*
  def move(command: Command): Option[Board] = robot.map { robot =>
    val newBoard = moveElement(robot, command).getOrElse(this)
    newBoard.printBoard(command)
    newBoard
  }

  def printBoard(command: Command) =
    println(s"Command: ${command.identifier}")
    println(data.grouped(dimension.width).map(_.mkString).mkString("\n"))
    println()
   */
  
  def findAll(boardElement: BoardElement): Seq[Position] = data.zipWithIndex.collect {
    case (c, index) if c == boardElement => index.position
  }

  private def robot = data.zipWithIndex.collectFirst {
    case (char, index) if char == BoardElements.ROBOT => index.position
  }

  private def moveElement(position: Position, command: Command): Option[Board] =
    val nextPosition = position + command
    this(nextPosition) match
      case BoardElements.WALL =>
        None
      case BoardElements.BOX =>
        for
         movedBox <- moveElement(nextPosition, command)
         movedElement <- movedBox.move(command)
        yield movedElement
      case BoardElements.OPEN_BOX =>
        for
          movedBoxClose <- moveElement(nextPosition + Command.Right, command)
          movedBoxOpen <- movedBoxClose.moveElement(nextPosition, command)
          movedElement <- movedBoxOpen.moveElement(position, command)
        yield movedElement
      case BoardElements.CLOSE_BOX =>
        for
          movedBoxOpen <- moveElement(nextPosition + Command.Left, command)
          movedBoxClose <- movedBoxOpen.moveElement(nextPosition, command)
          movedRobot <- movedBoxClose.moveElement(position, command)
        yield movedRobot
      case _ =>
        val positionValue = this(position)
        val nextPositionValue = this(nextPosition)
        val newData = data.updated(position.index, nextPositionValue).updated(nextPosition.index, positionValue)
        Some(copy(data = newData))

  extension (self: Int)
    def position: Position =
      val row = self / dimension.width
      val column = self - row * dimension.width
      Position(row, column)

  extension (self: Position)
    def index: Int = self.row * dimension.width + self.column

package day6

enum Direction(val idCharacter: Char):
  case Up extends Direction('^')
  case Right extends Direction('>')
  case Down extends Direction('v')
  case Left extends Direction('<')

  def next: Direction = this match
    case Direction.Up => Direction.Right
    case Direction.Right => Direction.Down
    case Direction.Down => Direction.Left
    case Direction.Left => Direction.Up
    
  def displacement: Displacement = this match
    case Direction.Up => Displacement(-1, 0)
    case Direction.Right => Displacement(0, 1)
    case Direction.Down => Displacement(1, 0)
    case Direction.Left => Displacement(0, -1)


object Direction:

  def fromCharacter(char: Char): Option[Direction] =
    Direction.values.find(_.idCharacter == char)

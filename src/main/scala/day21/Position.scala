package day21

import scala.annotation.targetName

case class Position(x: Int, y: Int):
  
  @targetName("add")
  def +(other: Position): Position = Position(x + other.x, y + other.y)
  
  @targetName("subtract")
  def -(other: Position): Position = Position(x - other.x, y - other.y)
  
  def projectOnX: Position = Position(x, 0)
  
  def projectOnY: Position = Position(0, y)

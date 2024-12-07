package day6

case class Displacement(x: Int, y: Int)

case class Point(x: Int, y: Int):
  
  def +(displacement: Displacement): Point =
    Point(x +  displacement.x, y + displacement.y)

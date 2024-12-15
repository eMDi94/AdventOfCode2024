package day14

import scala.annotation.tailrec
import scala.io.Source

@main def main(): Unit =
  val input = Source.fromResource("day14/input.txt").getLines
  given Board = Board(103, 101)
  val robots = input.collect {
    case s"p=$py,$px v=$vy,$vx" => Robot(Point(px.toInt, py.toInt), Point(vx.toInt, vy.toInt))
  }.toList

  val result = robots.map(_.at(100))
    .groupBy(_.quadrant)
    .filter(_._1.nonEmpty)
    .map(_._2.size)
    .product

  println(s"Part1 result is $result")

  val numberOfMoveForEasterEgg = searchEasterEgg(robots)
  println(s"Part2 result is $numberOfMoveForEasterEgg")


@tailrec
def searchEasterEgg(robots: List[Robot], numberOfMoves: Int = 1): Int =
  val updatedRobots = robots.map(_.move)
  val allDistinct = updatedRobots.distinctBy(_.currentPosition).size == robots.size // Each robot is in a different position
  if allDistinct then numberOfMoves else searchEasterEgg(updatedRobots, numberOfMoves + 1)

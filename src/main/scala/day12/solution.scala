package day12

import scala.io.Source

@main def main() =
  val input = Source.fromResource("day12/input.txt").getLines.toVector
  val height = input.size
  val width = input.head.length
  val data = input.flatten

  val board = Board(data, height, width)

  part1(board)
  part2(board)

def part1(board: Board): Unit =
  val result = board.regions.map(region => region.area * region.perimeter).sum
  println(s"Part1 Result is $result")

def part2(board: Board): Unit =
  val result = board.regions.map(region => region.area * region.sides).sum
  println(s"Part2 Result is $result")

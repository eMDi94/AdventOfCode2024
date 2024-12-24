package day20

import scala.io.Source

@main def part1(): Unit =
  parseInput("day20/input.txt") match
    case Some(raceTrack) =>
      val count = raceTrack.cheats(2).count(_.saved >= 100)
      println(s"The result is $count")
    case None =>
      println("Invalid input board")

@main def part2(): Unit =
  parseInput("day20/input.txt") match
    case Some(raceTrack) =>
      val count = raceTrack.cheats(20).count(_.saved >= 100)
      println(s"The result is $count")
    case None =>
      println("Invalid input board")

def parseInput(fileName: String) =
  val input = Source.fromResource(fileName).getLines.toVector
  val width = input.head.length
  val height = input.size
  given Dimension = Dimension(height, width)
  val board = Board(input.mkString.toVector)

  for
    start <- board.findElement('S')
    end <- board.findElement('E')
  yield RaceTrack(start, end, board)

package day18

import scala.io.Source


@main def part1(): Unit =
  val corruptedLocations = readCorruptedLocations("day18/input.txt")
  given Dimension = Dimension(71, 71)
  val taken = corruptedLocations.take(1024)

  val ram = Ram(taken)
  val start = Position(0, 0)
  val end = Position(70, 70)

  val solver = RamSolver(ram, start, end)
  solver.minimumPath match
    case Some(minimumPath) =>
      println(s"The result is ${minimumPath.size}")
    case None =>
      println("No path from start to end")

@main def part2(): Unit =
  val corruptedLocations = readCorruptedLocations("day18/input.txt")
  given Dimension = Dimension(71, 71)

  val start = Position(0, 0)
  val end = Position(70, 70)

  val firstNonSolvablePosition = Iterator.iterate(1024 -> corruptedLocations.size) { (i0, i1) => 
    val mid = (i0 + i1) / 2
    val ram = Ram(corruptedLocations.take(mid))
    val ramSolver = RamSolver(ram, start, end)
    ramSolver.minimumPath match
      case Some(_) => 
        mid + 1 -> i1
      case None => 
        i0 -> mid
  }.flatMap { (i0, i1) => 
    Option.when(i0 == i1)(corruptedLocations(i0 -1))
  }
    .next()

  println(s"The result is (${firstNonSolvablePosition.column},${firstNonSolvablePosition.row})")

def readCorruptedLocations(filePath: String) =
  Source.fromResource(filePath).getLines
    .map {
      case s"$y,$x" => Position(x.toInt, y.toInt)
    }
    .toVector

def printRamPath(corruptedLocations: Vector[Position], path: Vector[Position])(using dimension: Dimension) =
  val board = (0 until (dimension.width * dimension.height)).foldLeft(Vector.empty[Char]) { (acc, index) =>
    val row = index / dimension.width
    val column = index - row * dimension.width
    val p = Position(row, column)
    if corruptedLocations.contains(p) then acc :+ '#'
    else if path.contains(p) then acc :+ 'O'
    else acc :+ '.'
  }

  val stringBoard = board.grouped(dimension.width)
    .map(_.mkString)
    .mkString("\n")

  println(stringBoard)

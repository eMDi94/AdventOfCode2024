package day21

import scala.io.Source

def parseInput(fileName: String) = Source.fromResource(fileName).getLines.toList

// Part 1

@main def part1(): Unit =
  val input = parseInput("day21/input.txt")
  val result = input
    .filter(_.nonEmpty)
    .map { line =>
      val path = Iterator.iterate((line, true), 4)((currentLine, isNumeric) => (minPath(currentLine, isNumeric), false))
        .toList.last._1
      val lineNumber = line.init.toLong
      val pathLength = path.length()
      pathLength * lineNumber
    }
    .sum

  println(s"The solution is $result")

def minPathStep(from: Position, to: Position, positions: Set[Position]): String =
  val shift = to - from
  val horizontal = (if shift.x > 0 then ">" else "<") * shift.x.abs
  val vertical = (if shift.y > 0 then "v" else "^") * shift.y.abs
  val reverse = !positions(from + shift.projectOnX) || (positions(from + shift.projectOnY) && shift.x > 0)
  if reverse then vertical + horizontal + 'A' else horizontal + vertical + 'A'

def minPath(input: String, isNumeric: Boolean = false): String =
  val keypad = if isNumeric then numericKeypad else directionalKeypad
  val positions = if isNumeric then numericKeypadPositions else directionalKeypadPositions
  s"A$input".map(keypad).sliding(2).map(p => minPathStep(p(0), p(1), positions)).mkString


// Part 2

@main def part2(): Unit =
  val input = parseInput("day21/input.txt")
  val solver = new MemoizedPathSolver()
  val result = input
    .filter(_.nonEmpty)
    .map { line => solver.minPath(line, 0, 25) * line.init.toLong }
    .sum

  println(s"The result is $result")

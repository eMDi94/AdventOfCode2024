package day1


object Day1Application extends App:

  private enum Part:
    case Part1, Part2

  private val part = Part.Part2
  private val solver =
    part match {
      case Part.Part1 => Part1Solver(InputReader.readInput("day1/input.txt"))
      case Part.Part2 => Part2Solver(InputReader.readInput("day1/input.txt"))
    }

  println(s"The result is ${solver.result}")

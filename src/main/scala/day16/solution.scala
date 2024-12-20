package day16

import scala.io.Source


@main def part1(): Unit =
  val input = readInput("day16/input.txt")
  val reindeerState = ReindeerState(input)

  val result = Iterator.iterate(reindeerState)(_.nextState)
    .flatMap(_.minScore)
    .next()

  println(s"The result is $result")


@main def part2(): Unit =
  val input = readInput("day16/input.txt")
  val reindeerState = ReindeerState(input)

  val result = Iterator.iterate(reindeerState)(_.nextState)
    .flatMap(_.minPath)
    .next()

  println(s"The result is $result")


def readInput(file: String) = Source.fromResource(file).getLines.toVector

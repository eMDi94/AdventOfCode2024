package day22

import scala.io.Source

// Part 1

@main def part1(): Unit =
  val result = readInput("day22/input.txt")
    .foldMap(line => line.toLong.secretsIterator.nth(2000))

  println(s"The result is $result")

// Part2

@main def part2(): Unit =
  val result = readInput("day22/input.txt")
    .foldMap(deltaMap)
    .values.max

  println(s"The result is $result")

type QuartupleDifference = (Long, Long, Long, Long)

def deltaMap(line: String): Map[QuartupleDifference, Long] =
  given Semigroup[Long] = leftBiasedSemigroup
  line.toLong.secretsIterator.map(_ % 10).take(2000).sliding(5).foldMap { quintuple =>
    Map(deltaQuartuple(quintuple) -> quintuple(4))
  }

def deltaQuartuple(values: Seq[Long]): QuartupleDifference =
  (values(1) - values.head, values(2) - values(1), values(3) - values(2), values(4) -  values(3))

// Common

def readInput(fileName: String) =
  Source.fromResource(fileName).getLines

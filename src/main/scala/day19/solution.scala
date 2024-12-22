package day19

import scala.io.Source


@main def part1(): Unit =
  val (availablePatterns, toBuildPatterns) = parseInput("day19/input.txt")
  val patternsSolver = PatternsSolver(availablePatterns)
  val result = toBuildPatterns.count(patternsSolver.makePattern(_) > 0)
  println(s"The result is $result")


@main def part2(): Unit =
  val (availablePatterns, toBuildPatterns) = parseInput("day19/input.txt")
  val patternsSolver = PatternsSolver(availablePatterns)
  val result = toBuildPatterns.map(patternsSolver.makePattern).sum
  println(s"The result is $result")

def parseInput(fileName: String) =
  val input = Source.fromResource(fileName).mkString
  val (availablePatterns, toBuildPatterns) = input.split("\n\n") match
    case Array(ap, tbp) => (ap.split(",").map(_.trim).toVector, tbp.split("\n"))

  (availablePatterns, toBuildPatterns)

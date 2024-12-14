package day13

import scala.io.Source

extension (l: Long)
  def divideOpt(other: Long): Option[Long] = Option.when(other != 0 && l % other == 0)(l / other)

case class LinearSystem(xa: Long, ya: Long, xb: Long, yb: Long, px: Long, py: Long):

  def solve: Option[(Long, Long)] =
    val bNum = (xa * py) - (ya * px)
    val bDen = (xa*yb) - (ya*xb)
    val bOpt = bNum.divideOpt(bDen)
    val aOpt = bOpt.flatMap(b => (py - (b * yb)).divideOpt(ya))
    for
      aLong <- aOpt
      bLong <- bOpt
    yield (aLong, bLong)

@main def main(): Unit =
  val input = Source.fromResource("day13/input.txt").mkString.split("\n\n")

  part1(input)
  part2(input)


def part1(input: Array[String]): Unit =
  val systems = input.map {
    case s"Button A: X+$xa, Y+$ya\nButton B: X+$xb, Y+$yb\nPrize: X=$px, Y=$py" =>
      LinearSystem(xa.toLong, ya.toLong, xb.toLong, yb.toLong, px.toLong, py.toLong)
  }.toList

  solveSystemsAndPrint("Part1", systems)


def part2(input: Array[String]): Unit =
  val systems = input.map {
    case s"Button A: X+$xa, Y+$ya\nButton B: X+$xb, Y+$yb\nPrize: X=$px, Y=$py" =>
      LinearSystem(xa.toLong, ya.toLong, xb.toLong, yb.toLong, px.toLong + 10000000000000L, py.toLong + 10000000000000L)
  }.toList

  solveSystemsAndPrint("Part2", systems)


def solveSystemsAndPrint(part: String, systems: List[LinearSystem]): Unit =
  val tokens = systems.map { system =>
    system.solve match
      case Some((a, b)) => 3 * a + 1 * b
      case _ => 0
  }.sum

  println(s"$part result is $tokens")
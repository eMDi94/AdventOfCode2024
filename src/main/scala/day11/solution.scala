package day11

import scala.io.Source
import collection.mutable.Map as MutableMap

type Stone = Long
type Depth = Int
type MemoizationKey = (Depth, Stone)
type MemoizationMap = MutableMap[MemoizationKey, Long]

@main def main(): Unit =
  val input = Source.fromResource("day11/input.txt").mkString
  val stones = input.split(" ").filter(_.nonEmpty).map(_.toLong).toList
  
  part1(stones)
  part2(stones)


def part1(stones: List[Stone]): Unit =
  val result = (0 until 25).foldLeft(stones) { (currentStones, _) =>
    currentStones.flatMap(blink)
  }.size
  
  println(s"Part1 Result: $result")


def part2(stones: List[Stone]): Unit =
  def recursiveHelper(depth: Int, stone: Long, memo: MemoizationMap): Long =
    (depth, stone) match
      case (75, _) => 1 // 1 Stone
      case x =>
        memo.getOrElseUpdate(x, blink(stone).map(recursiveHelper(depth + 1, _, memo)).sum)

  val memoMap = MutableMap.empty[MemoizationKey, Long]
  val result = stones.map(recursiveHelper(0, _, memoMap)).sum
  println(s"Part2 Result: $result")

def blink(stone: Stone): Seq[Stone] = stone match
  case 0 => Seq(1)
  case n if hasEvenNumberOfDigits(n) =>
    val stringNumber = n.toString
    val index = stringNumber.length / 2
    val (left, right) = stringNumber.splitAt(index)
    Seq(
      left.toLong,
      right.toLong
    )
  case _ =>
    Seq(stone * 2024)

private def hasEvenNumberOfDigits(number: Stone) = number.toString.length % 2 == 0


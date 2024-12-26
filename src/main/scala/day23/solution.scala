package day23

import scala.io.Source

@main def part1(): Unit =
  val connections = parseFile("day23/input.txt")
  val lan = LANConnections(connections)

  val count = lan.allComputers.combinations(3).count { combination =>
    val startsWith = combination.exists(_.head == 't')
    val comb = combination.tails.forall {
      case currentComputer :: Nil =>
        true
      case currentComputer :: otherComputers =>
        val set = lan.computersMap(currentComputer)
        otherComputers.forall(set)
      case Nil =>
        true
    }
    startsWith && comb
  }

  println(s"The result is $count")

@main def part2(): Unit =
  val connections = parseFile("day23/input.txt")
  val lan = LANConnections(connections)

  val result = lan.maximumClique.toList.sorted.mkString(",")

  println(s"The result is: $result")

def parseFile(fileName: String) =
  Source.fromResource(fileName)
    .getLines
    .map {
      case s"$c1-$c2" => (c1, c2)
    }
    .toList

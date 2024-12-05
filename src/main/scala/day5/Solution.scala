package day5

import scala.annotation.tailrec
import scala.io.Source

type NumbersOrdering = Map[Int, List[Int]]

@main def main(): Unit =
  val input = Source.fromResource("day5/input.txt").getLines.mkString("\n")
  input.split("\n\n").toList match {
    case orderings :: toProduce :: Nil =>
      given NumbersOrdering = parseOrderings(orderings)
      val pagesToProduce = parseToProduceLines(toProduce)

      /* Part1
      val result = pagesToProduce.filter(isValidLine).map { validLine =>
        val middle = validLine.size / 2
        validLine(middle)
      }
        .sum

      println(s"The result is $result")
       */

      val result = pagesToProduce.filterNot(isValidLine).map { invalidLine =>
        val validLine = orderLine(invalidLine)
        val middle = validLine.size / 2

        validLine(middle)
      }
        .sum

      println(s"The result is $result")
    case _ =>
      println("Invalid input")
  }

def parseOrderings(orderings: String): NumbersOrdering =
  orderings.split("\n").foldLeft(Map.empty){ (acc, line) =>
    line.split("\\|").filter(_.nonEmpty).map(_.toInt).toList match {
      case first :: second :: Nil =>
        val newList = acc.getOrElse(second, List.empty) :+ first
        acc ++ Map(second -> newList)
      case _ => acc
    }
  }

def parseToProduceLines(toProduce: String) =
  toProduce.split("\n")
    .map(_.split(",").filter(_.nonEmpty).map(_.toInt).toList)
    .toList

def isValidLine(line: List[Int])(using pageOrderings: NumbersOrdering): Boolean =
  line match {
    case Nil => false
    case head :: Nil => false
    case head :: tail => checkElement(head, tail)
  }

@tailrec
def checkElement(currentElement: Int, nextElements: List[Int])(using pageOrderings: NumbersOrdering): Boolean =
  nextElements match {
    case Nil => true
    case _ =>
      val result = pageOrderings.get(currentElement).forall(_.intersect(nextElements).isEmpty)
      if (result) checkElement(nextElements.head, nextElements.tail) else false
  }

def orderLine(line: List[Int])(using pageOrderings: NumbersOrdering): List[Int] =
  line.sortWith { (v1, v2) =>
    pageOrderings.get(v1).forall(_.contains(v2))
  }

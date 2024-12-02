package day1

import scala.io.Source

object InputReader:
  
  def readInput(filePath: String) =
    Source.fromResource(filePath)
      .getLines
      .foldLeft((List.empty[Int], List.empty[Int])) { (acc, line) =>
        line.split("\\s").filter(_.nonEmpty) match {
          case Array(first, second) =>
            (acc._1 :+ first.toInt, acc._2 :+ second.toInt)
          case _ =>
            acc
        }
      }

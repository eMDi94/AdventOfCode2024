package day9

import scala.io.Source
import DiskBlock._


@main def main(): Unit =
  val input = Source.fromResource("day9/input.txt").getLines.mkString.map(_.asDigit)
  val diskAllocation = input.grouped(2).zipWithIndex.flatMap {
    case (Seq(fileLength, freeLength), id) => List(File(id, fileLength), FreeSpace(freeLength))
    case (Seq(fileLength), id) => List(File(id, fileLength))
  }.toList

  val fileSystem = FileSystem(diskAllocation)

  println(s"The dense checksum is ${fileSystem.denseChecksum}")
  println(s"The sparse checksum is ${fileSystem.sparseChecksum}")
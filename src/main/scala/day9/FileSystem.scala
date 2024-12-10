package day9

import scala.annotation.tailrec
import DiskBlock._

case class FileSystem(blocks: List[DiskBlock]):

  def denseChecksum: Long = checksum(denseFragmentation)
  def sparseChecksum: Long = checksum(sparseFragmentation)

  private def denseFragmentation =
    @tailrec
    def recursiveHelper(currentBlocks: List[DiskBlock], acc: List[DiskBlock] = List.empty): List[DiskBlock] =
      currentBlocks match
        case (f@File(_, _)) :: tail => recursiveHelper(tail, f +: acc)
        case (init@FreeSpace(_) +: _) :+ FreeSpace(_) => recursiveHelper(init, acc)
        case FreeSpace(s) +: middle :+ (f@File(_, l)) if s == l => recursiveHelper(middle, f +: acc)
        case FreeSpace(s) +: middle :+ (f@File(_, l)) if s > l => recursiveHelper(FreeSpace(s - l) +: middle, f +: acc)
        case FreeSpace(s) +: middle :+ File(id, l) if s < l => recursiveHelper(middle :+ File(id, l - s), File(id, s) +: acc)
        case _ => acc

    recursiveHelper(blocks).reverse

  private def sparseFragmentation =
    @tailrec
    def recursiveHelper(currentBlocks: List[DiskBlock], acc: List[DiskBlock] = List.empty): List[DiskBlock] =
      currentBlocks match
        case init :+ (f @ FreeSpace(_)) => recursiveHelper(init, f +: acc)
        case init :+ (f @ File(_, fileLength)) =>
          val separatedList = init.span  {
            case FreeSpace(freeSpaceLength) => freeSpaceLength < fileLength
            case _ => true
          }
          separatedList match
            case (_, Nil) => recursiveHelper(init, f +: acc)
            case (before, FreeSpace(s) +: after) if s == fileLength => recursiveHelper((before :+ f) ++ after, FreeSpace(fileLength) :: acc)
            case (before, FreeSpace(s) +: after) => recursiveHelper((before :+ f :+ FreeSpace(s - fileLength)) ++ after, FreeSpace(fileLength) :: acc)
        case _ => acc
    
    recursiveHelper(blocks)

  private def checksum(blocks: List[DiskBlock]) =
    blocks.foldLeft((0L, 0)) {
      case ((acc, idx), File(id, length)) =>
        val endRange = idx + length - 1
        val sumOfRange = ((idx + endRange) / 2.0) * (endRange - idx + 1) * id
        (acc + sumOfRange.toLong, idx + length)
      case ((acc, idx), FreeSpace(length)) =>
        (acc, idx + length)
    }._1

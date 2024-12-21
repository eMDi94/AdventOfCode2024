package day18

import scala.annotation.tailrec

case class RamSolver(ram: Ram, start: Position, end: Position):

  def minimumPath: Option[Vector[Position]] =
    @tailrec
    def recursiveHelper(priorityQueue: PriorityQueue[Int, Position], distances: Map[Position, Int], fromMap: Map[Position, Position]): Option[Map[Position, Position]] =
      if priorityQueue.isEmpty then None
      else
        val (priority, head, rest) = priorityQueue.dequeue
        head match
          case `end` => Some(fromMap)
          case currentPosition =>
            val newScore = priority + 1
            val (newQueue, newDistances, newFromToMap) = currentPosition.neighbours
              .filter(loc => ram.inRam(loc) && ram(loc) != '#')
              .foldLeft((rest, distances, fromMap)) { (acc, neighbour) =>
              val (queue, distances, fromToMap) = acc
              if newScore < distances.getOrElse(neighbour, Int.MaxValue) then
                (queue.enqueue(newScore, neighbour), distances + (neighbour -> newScore), fromToMap + (neighbour -> currentPosition))
              else
                acc
            }
            recursiveHelper(newQueue, newDistances, newFromToMap)

    val priorityQueue = PriorityQueue(0, start)
    recursiveHelper(priorityQueue, Map.empty, Map.empty).map(reconstructPath)

  private def reconstructPath(fromMap: Map[Position, Position]) =
    @tailrec
    def recursiveHelper(currentPosition: Position, path: Vector[Position] = Vector.empty): Vector[Position] =
      fromMap(currentPosition) match
        case `start` =>
          (path :+ start).reverse
        case nextPosition =>
          recursiveHelper(nextPosition, path :+ currentPosition)

    recursiveHelper(end)

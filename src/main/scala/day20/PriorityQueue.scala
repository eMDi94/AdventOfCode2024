package day20

import scala.collection.immutable.TreeMap


type PriorityQueue[K, V] = TreeMap[K, Vector[V]]

object PriorityQueue:
  def apply[K: Ordering, V](key: K, value: V): PriorityQueue[K, V] =
    TreeMap(key -> Vector(value))

extension [K, V](self: PriorityQueue[K, V])
  def enqueue(key: K, value: V): PriorityQueue[K, V] =
    self.updatedWith(key) {
      case Some(values) => Some(values :+ value)
      case None => Some(Vector(value))
    }

  def dequeue: (K, V, PriorityQueue[K, V]) =
    val (priority, values) = self.head
    if values.size == 1 then (priority, values.head, self - priority)
    else (priority, values.head, self + (priority -> values.tail))

  def firstValue: V = firstValues.head

  def firstValues: Vector[V] = self.valuesIterator.next()

package day16

import scala.collection.immutable.TreeMap


trait Priority[K, V]:
  def priority(value: V): K


type PriorityQueue[K, V] = TreeMap[K, Vector[V]]

object PriorityQueue:
  def apply[K: Ordering, V](value: V)(using P: Priority[K, V]): PriorityQueue[K, V] =
    TreeMap(P.priority(value) -> Vector(value))

extension [K, V](self: PriorityQueue[K, V])
  def enqueue(value: V)(using P: Priority[K, V]): PriorityQueue[K, V] =
    self.updatedWith(P.priority(value)) {
      case Some(values) => Some(values :+ value)
      case None => Some(Vector(value))
    }

  def enqueueAll(values: Iterable[V])(using P: Priority[K, V]): PriorityQueue[K, V] =
    values.foldLeft(self)(_.enqueue(_))

  def dequeue: (V, PriorityQueue[K, V]) =
    val (priority, values) = self.head
    if values.size == 1 then (values.head, self - priority)
    else (values.head, self + (priority -> values.tail))

  def firstValue: V = firstValues.head

  def firstValues: Vector[V] = self.valuesIterator.next()

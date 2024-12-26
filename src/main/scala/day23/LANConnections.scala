package day23

type Computer = String
type Connection = (Computer, Computer)

case class LANConnections(connections: List[Connection]):

  val allComputers: List[Computer] = connections.flatMap((c1, c2) => Seq(c1, c2)).distinct
  val computersMap: Map[Computer, Set[Computer]] =
    connections.flatMap((c1, c2) => List((c1, c2), (c2, c1))).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

  def maximumClique: Set[Computer] =
    def maximalCliques(r: Set[String], p: Set[String], x: Set[String]): Set[Set[String]] =
      if p.isEmpty && x.isEmpty then
        Set(r)
      else
        val u = p.union(x).head
        p.diff(computersMap(u)).foldLeft((Set[Set[String]](), p, x)):
          case ((res, p, x), v) =>
            (res ++ maximalCliques(r.incl(v), p.intersect(computersMap(v)), p.intersect(computersMap(v))), p - v, x.incl(v))
        ._1

    maximalCliques(Set(), computersMap.keySet, Set()).maxBy(_.size)

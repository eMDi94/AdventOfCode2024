package day16

import day16.Direction.East

case class Reindeer(score: Int, position: Position, direction: Direction, path: Vector[Position]):

  def neighbours: Vector[Reindeer] = Vector(
    Reindeer(score + 1, position + direction, direction, path :+ (position + direction)),
    Reindeer(score + 1000, position, direction.clockwise, path),
    Reindeer(score + 1000, position, direction.counterClockwise, path)
  )

given Priority[Int, Reindeer] = _.score


case class ReindeerState(maze: Maze, end: Position, queue: PriorityQueue[Int, Reindeer], visited: Set[(Position, Direction)]):

  def nextState: ReindeerState =
    val (reindeer, rest) = queue.dequeue
    val neighbours = reindeer.neighbours.filter(next => maze(next.position) != '#' && !visited.contains(next.position -> next.direction))
    ReindeerState(maze, end, rest.enqueueAll(neighbours), visited + (reindeer.position -> reindeer.direction))

  def minScore: Option[Int] = Option(queue.firstValue).filter(_.position == end).map(_.score)
  def minPath: Option[Int] = Option.when(queue.firstValue.position == end)(queue.firstValues.filter(_.position == end).flatMap(_.path).distinct.size)


object ReindeerState:

  def apply(input: Vector[String]): ReindeerState =
    val height = input.size
    val width = input.head.length
    given Dimension = Dimension(height, width)
    val maze = Maze(input.mkString.toVector)
    val start = maze.findPosition('S').get
    val end = maze.findPosition('E').get
    val reindeer = Reindeer(0, start, East, Vector(start))
    new ReindeerState(maze, end, PriorityQueue(reindeer), Set.empty)

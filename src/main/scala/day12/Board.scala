package day12

import day12.Direction.{East, North}

enum Direction(val row: Int, val column: Int):
  case North extends Direction(-1, 0)
  case East extends Direction(0, 1)
  case South extends Direction(1, 0)
  case West extends Direction(0, -1)
  
  def toLocation: Location = Location(row, column)

case class Location(row: Int, column: Int):

  import Location._

  def +(other: Location): Location = Location(row + other.row, column + other.column)
  def +(direction: Direction): Location = this + direction.toLocation
  def neighbours: Seq[Location] = Direction.values.map(this + _)

case class Region(locations: Set[Location]):
  def contains(location: Location): Boolean = locations.contains(location)
  def +(location: Location): Region = Region(locations + location)
  def apply(location: Location): Boolean = locations(location)
  def area: Int = locations.size
  def perimeter: Int = locations.toList.map(loc => loc.neighbours.count(neighbour => !locations(neighbour))).sum
  def groups: Set[Region] = locations.foldLeft(Set.empty[Region]) { (regions, location) =>
    val (toMerge, notTouching) = regions.partition(region => location.neighbours.exists(region.contains))
    notTouching + (toMerge + Region(Set(location))).reduce((s1, s2) => Region(s1.locations ++ s2.locations))
  }
  def sides: Int =
    val north = Region(locations.filter(loc => !locations.contains(loc + North))).groups.size
    val east = Region(locations.filter(loc => !locations.contains(loc + East))).groups.size
    val south = Region(locations.filter(loc => !locations.contains(loc + Direction.South))).groups.size
    val west = Region(locations.filter(loc => !locations.contains(loc + Direction.West))).groups.size
    north + east + south + west

object Region:

  def empty: Region = Region(Set.empty)

case class Board(data: Vector[Char], height: Int, width: Int):

  import Board._

  def apply(row: Int, column: Int): Char = data(width * row + column)
  def apply(location: Location): Char = this(location.row, location.column)

  def regions: Vector[Region] =
    boardIndices.foldLeft(Vector.empty[Region]) { (regs, location) =>
      if regs.exists(_.contains(location)) then regs
      else regs :+ explore(this(location), location, Region.empty)
    }

  private def explore(char: Char, location: Location, region: Region): Region =
    location.neighbours.filter(inBoard).foldLeft(region + location) { (region, neighbour) =>
      if region(neighbour) || this(neighbour) != char then region
      else explore(char, neighbour, region)
    }

  private val inBoard: Function[Location, Boolean] = {
    case Location(row, column) => row >= 0 && row < height && column >= 0 && column < width
  }

  private def boardIndices = for
    i <- 0 until height
    j <- 0 until width
  yield Location(i, j)

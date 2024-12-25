package day21

import scala.collection.mutable as mut

class MemoizedPathSolver {

  private type Level = Int

  private val numericKeypad = Map(
    '7' -> Position(0, 0), '8' -> Position(1, 0), '9' -> Position(2, 0),
    '4' -> Position(0, 1), '5' -> Position(1, 1), '6' -> Position(2, 1),
    '1' -> Position(0, 2), '2' -> Position(1, 2), '3' -> Position(2, 2),
    '0' -> Position(1, 3), 'A' -> Position(2, 3),
  )
  private val numericKeypadPositions = numericKeypad.values.toSet

  private val directionalKeypad = Map(
    '^' -> Position(1, 0), 'A' -> Position(2, 0),
    '<' -> Position(0, 1), 'v' -> Position(1, 1), '>' -> Position(2, 1),
  )
  private val directionalKeypadPositions = directionalKeypad.values.toSet

  private val cache = mut.Map.empty[(Position, Position, Level, Level), Long]

  def minPath(input: String, level: Level, maxLevel: Level): Long =
    val keypad = if level == 0 then numericKeypad else directionalKeypad
    s"A$input".map(keypad).sliding(2).map(p => minPathStep(p(0), p(1), level, maxLevel)).sum

  private def minPathStep(from: Position, to: Position, level: Level, maxLevel: Level): Long =
    cache.getOrElseUpdate((from, to, level, maxLevel), {
      val positions = if level == 0 then numericKeypadPositions else directionalKeypadPositions
      val shift = to - from
      val h = (if shift.x > 0 then ">" else "<") * shift.x.abs
      val v = (if shift.y > 0 then "v" else "^") * shift.y.abs
      val reverse = !positions(from + shift.projectOnX) || (positions(from + shift.projectOnY) && shift.x > 0)
      val res = if reverse then v + h + 'A' else h + v + 'A'
      if level == maxLevel then res.length else minPath(res, level + 1, maxLevel)
    })

}

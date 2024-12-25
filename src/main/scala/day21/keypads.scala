package day21

val numericKeypad = Map(
  '7' -> Position(0, 0), '8' -> Position(1, 0), '9' -> Position(2, 0),
  '4' -> Position(0, 1), '5' -> Position(1, 1), '6' -> Position(2, 1),
  '1' -> Position(0, 2), '2' -> Position(1, 2), '3' -> Position(2, 2),
  '0' -> Position(1, 3), 'A' -> Position(2, 3),
)
val numericKeypadPositions = numericKeypad.values.toSet

val directionalKeypad = Map(
  '^' -> Position(1, 0), 'A' -> Position(2, 0),
  '<' -> Position(0, 1), 'v' -> Position(1, 1), '>' -> Position(2, 1),
)
val directionalKeypadPositions = directionalKeypad.values.toSet

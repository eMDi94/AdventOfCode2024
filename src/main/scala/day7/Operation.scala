package day7

enum Operation:
  case Add, Multiply, Concatenation

  def applyOperation(first: Long, second: Long): Long = this match
    case Operation.Add => first + second
    case Operation.Multiply => first * second
    case Operation.Concatenation => (first.toString + second.toString).toLong

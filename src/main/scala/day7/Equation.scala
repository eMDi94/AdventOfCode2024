package day7


case class Equation(result: Long, numbers: List[Long]):

  def isSolvable: Boolean = applyOperators(numbers.head, numbers.tail)

  private def applyOperators(currentPartial: Long, remainingElements: List[Long]): Boolean =
    remainingElements match
      case head :: tail =>
        Operation.values.exists { operation =>
          val newPartial = operation.applyOperation(currentPartial, head)
          applyOperators(newPartial, tail)
        }
      case Nil =>
        currentPartial == result

package day7

extension (o: Operation)
  def next: Option[Operation] = o match
    case Operation.Add => Some(Operation.Multiply)
    case Operation.Multiply => None


def parseLine(line: String): Option[Equation] =
  line.split(":") match
    case Array(result, numbers, _*) =>
      val parsedNumbers = numbers.split(" ").filter(_.nonEmpty).map(_.trim.toLong)
      Some(Equation(result.trim.toLong, parsedNumbers.toList))
    case _ => None

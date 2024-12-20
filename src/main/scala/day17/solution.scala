package day17

import scala.annotation.tailrec
import scala.io.Source

extension (n: Long)
  @tailrec
  def dropz: Long = if n % 8 != 0 then n else (n >> 3).dropz

@main def part1(): Unit =
  val calculator = parseCalculator("day17/input.txt")

  val result = execute(calculator)

  println(s"The result is ${result.mkString(",")}")


@main def part2(): Unit =
  val calculator = parseCalculator("day17/input.txt")

  val result = Iterator.iterate(1L) { registryA =>
    if calculator.program.endsWith(execute(calculator.copy(registryA = registryA))) then registryA << 3
    else (registryA + 1).dropz
  }.flatMap(registryA => Option.when(execute(calculator.copy(registryA = registryA)) == calculator.program)(registryA))
    .next()

  println(s"The result is $result")

def parseCalculator(fileName: String): Calculator =
  val input = Source.fromResource("day17/input.txt").getLines.mkString("\n")
  val (registries, program) = input.split("\n\n") match
    case Array(reg, prg) => (reg, prg)

  val (a, b, c) = registries match
    case s"Register A: $a\nRegister B: $b\nRegister C: $c" => (a.toInt, b.toInt, c.toInt)

  val programList = program match
    case s"Program: $p" => p.split(",").toVector.map(_.trim.toInt)
  Calculator(a, b, c, programList)

def execute(calculator: Calculator) =
  Iterator.iterate(calculator)(_.nextStep)
    .flatMap(_.output)
    .next()

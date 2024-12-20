package day17

case class InvalidProgram(message: String, cause: Option[Throwable] = None) extends Throwable(message, cause.orNull)

enum Operation:
  case adv,bxl,bst,jnz,bxc,out,bdv,cdv

case class Calculator(registryA: Long, registryB: Long, registryC: Long, program: Vector[Int],
                      programCounter: Int = 0, outAcc: Vector[Long] = Vector.empty):

  import Operation.*

  def nextStep: Calculator =
    (Operation.fromOrdinal(program(programCounter)), program(programCounter + 1)) match
      case (Operation.adv, operand) =>
        copy(registryA = registryA >> comboValue(operand), programCounter = programCounter + 2)
      case (Operation.bxl, operand) =>
        copy(registryB = registryB ^ operand, programCounter = programCounter + 2)
      case (Operation.bst, operand) =>
        copy(registryB = comboValue(operand) % 8, programCounter = programCounter + 2)
      case (Operation.jnz, operand) =>
        copy(programCounter = if registryA == 0 then programCounter + 2 else operand)
      case (Operation.bxc, _) => 
        copy(registryB = registryB ^ registryC, programCounter = programCounter + 2)
      case (Operation.out, operand) => 
        copy(outAcc = outAcc :+ (comboValue(operand) % 8).toInt, programCounter = programCounter + 2)
      case (Operation.bdv, operand) => 
        copy(registryB = registryA >> comboValue(operand), programCounter = programCounter + 2)
      case (Operation.cdv, operand) => 
        copy(registryC = registryA >> comboValue(operand), programCounter = programCounter + 2)

  def output: Option[Vector[Long]] = Option.when(programCounter == program.size)(outAcc)

  private def comboValue(operand: Long) =
    operand match
      case 4 => registryA
      case 5 => registryB
      case 6 => registryC
      case o => o

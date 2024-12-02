package day1

class Part1Solver(input: (List[Int], List[Int])) extends BaseSolver(input):
  
  override val result: Int =
    list1.zip(list2)
      .map((v1, v2) => Math.abs(v1 - v2))
      .sum

package day1

class Part2Solver(input: (List[Int], List[Int])) extends BaseSolver(input):
  
  override val result: Int =
    list1.map { val1 =>
        val count = list2.count(_ == val1)
        val1 * count
      }
      .sum

package day1

abstract class BaseSolver(input: (List[Int], List[Int])):

  protected val list1: Seq[Int] = input._1.sorted
  protected val list2: Seq[Int] = input._2.sorted

  val result: Int

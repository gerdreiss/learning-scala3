package numbers

object LargestNumber extends App:

  def solution(numbers: List[Int]): String =
    // my solution, does not work
    // numbers.map(_.toString).sorted.reverse.mkString
    given Ordering[Int] = Ordering.fromLessThan { (a, b) =>
      // concatenate a with b => ab
      // concatenate b with a => ba
      // compare the results lexicographically
      (s"$a$b" compareTo s"$b$a") >= 0
    }

    numbers.sorted.mkString

  println(solution(List(0, 0, 0)))
  println(solution(List(10, 2)))
  println(solution(List(3, 30, 5, 9, 34)))
  println(solution(List(2020, 20, 1010, 10, 2, 22)))
  println(solution(List(1)))
  println(solution(List()))

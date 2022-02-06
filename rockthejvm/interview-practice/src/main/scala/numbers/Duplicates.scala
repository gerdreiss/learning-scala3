package numbers

import scala.annotation.tailrec
import scala.collection.mutable

object Duplicates extends App:

  // receive a list of dupes, except exactly one
  def solutionG(nums: List[Int]): Int =
    nums.groupBy(identity).filter((_, group) => group.size == 1).values.flatten.head

  def solutionDan(nums: List[Int]): Int =
    // Complexity: O(N^2) time, O(1) space
    @tailrec
    def naive(remainder: List[Int]): Int =
      if remainder.isEmpty then throw IllegalStateException("no unique num")
      else
        val count = nums.count(_ == remainder.head)
        if count == 1 then remainder.head
        else naive(remainder.tail)

    // Complexity: O(N) time, O(N) space
    @tailrec
    def naiveWithMem(remainder: List[Int], occurrences: Map[Int, Int] = Map.empty): Int =
      if remainder.isEmpty then occurrences.filter((_, count) => count == 1).head._1
      else
        val current = remainder.head
        naiveWithMem(
          remainder.tail,
          occurrences + (current -> (1 + occurrences.getOrElse(current, 0)))
        )

    // Complexity: O(N) time, O(N) space with some optimizations
    @tailrec
    def withLessMem(remainder: List[Int], mem: Set[Int] = Set.empty): Int =
      if remainder.isEmpty then mem.head
      else if mem.contains(remainder.head) then withLessMem(remainder.tail, mem - remainder.head)
      else withLessMem(remainder.tail, mem + remainder.head)

    // optimal solution:
    // trick: ^ = XOR
    // 0 ^ 0 == 0
    // 0 ^ 1 == 1
    // 1 ^ 0 == 1
    // 1 ^ 1 == 0
    // Complexity: O(N) time, O(1) space
    nums.fold(0)(_ ^ _)

  println(solutionDan(List(1, 1, 2, 2, 3, 4, 4)))

  val aux     = (1 to 100000).toList
  val numbers = aux ++ List(2039234) ++ aux

  val startDan = System.currentTimeMillis
  val resDan   = solutionDan(numbers)
  println(s"Dan's solution $resDan took ${System.currentTimeMillis - startDan} millis")

  val startG = System.currentTimeMillis
  val resG   = solutionG(numbers)
  println(s"G's solution $resG took ${System.currentTimeMillis - startG} millis")

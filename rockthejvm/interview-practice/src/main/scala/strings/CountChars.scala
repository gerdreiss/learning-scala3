package strings

import scala.annotation.tailrec

object CountChars extends App:

  def solutionG(s: String): Map[Char, Int] =
    s.groupBy(identity).mapValues(_.length).toMap

  def solutionDan(s: String): Map[Char, Int] =
    @tailrec
    def rec(remaining: String, acc: Map[Char, Int] = Map.empty): Map[Char, Int] =
      if remaining.isEmpty then acc
      else if acc.contains(remaining.head) then
        val currentOccurrences = acc(remaining.head)
        rec(remaining.tail, acc + (remaining.head -> (currentOccurrences + 1)))
      else rec(remaining.tail, acc + (remaining.head -> 1))

    rec(s)

  println(solutionDan("Scala"))
  println(solutionG("Scala"))

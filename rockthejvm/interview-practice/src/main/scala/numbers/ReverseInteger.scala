package numbers

import java.math.BigInteger
import scala.annotation.tailrec

object ReverseInteger extends App:

  // return a number with the digits reversed
  // if the result overflows Int, return 0
  def solutionG(n: Int): Int =
    if n == Int.MinValue then 0
    else
      val r = BigInt(math.abs(n).toString.reverse, 10)
      if r > BigInt(Int.MaxValue) then 0 else r.intValue * math.signum(n)

  def solutionDan(n: Int): Int =
    @tailrec
    def reverse(remaining: Int, acc: Int): Int =
      if remaining == 0 then acc
      else
        val digit     = remaining % 10
        val tentative = acc * 10 + digit
        if (acc >= 0) != (tentative >= 0) then 0
        else reverse(remaining / 10, tentative)

    if n == Int.MinValue then 0
    else if n < 0 then -reverse(-n, 0)
    else reverse(n, 0)

  println("Dan's solution:")
  println("Positives")
  println(solutionDan(0))
  println(solutionDan(9))
  println(solutionDan(53))
  println(solutionDan(504))
  println(solutionDan(540))
  println(solutionDan(1241123))
  println(solutionDan(Int.MaxValue))
  println("Negatives")
  println(solutionDan(-9))
  println(solutionDan(-53))
  println(solutionDan(-504))
  println(solutionDan(-540))
  println(solutionDan(-1241123))
  println(solutionDan(Int.MinValue))

  println("G's solution:")
  println("Positives")
  println(solutionG(0))
  println(solutionG(9))
  println(solutionG(53))
  println(solutionG(504))
  println(solutionG(540))
  println(solutionG(1241123))
  println(solutionG(Int.MaxValue))
  println("Negatives")
  println(solutionG(-9))
  println(solutionG(-53))
  println(solutionG(-504))
  println(solutionG(-540))
  println(solutionG(-1241123))
  println(solutionG(Int.MinValue))

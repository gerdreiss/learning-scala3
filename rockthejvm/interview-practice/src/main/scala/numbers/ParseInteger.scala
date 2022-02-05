package numbers

import scala.annotation.tailrec

object ParseInteger extends App:

  /**
   * Return a number from the string argument:
   * - there may be leading spaces, ignore those
   * - read the sign character if present
   * - read all the digits until the end of the string, or until a non-digit character
   * - return the number formed from those digits
   * - if the number exceeds the int range, return either Int.MinValue (underflow) or a Int.MaxValue (overflow)
   */
  def solutionG(s: String): Int =
    val toParse = s.trim
    val signum  = if toParse.startsWith("-") then -1L else 1L
    val parsed  = signum * toParse
      .dropWhile(ch => ch == '+' || ch == '-')
      .takeWhile(_.isDigit)
      .foldLeft(0L) { (acc, next) =>
        acc * 10 + next
      }

    if parsed > Int.MaxValue.toLong then Int.MaxValue
    else if parsed < Int.MinValue.toLong then Int.MinValue
    else parsed.toInt

  @tailrec
  def solutionDan(s: String): Int =
    val digits = "0123456789".toSet

    def integerRangeEnd(signum: Int): Int =
      if signum >= 0 then Int.MaxValue else Int.MinValue

    @tailrec
    def rec(remainder: String, signum: Int, acc: Int): Int =
      if remainder.isEmpty || !digits.contains(remainder.charAt(0)) then acc
      else
        val newDigit  = remainder.charAt(0) - '0'
        val tentative = acc * 10 + newDigit * signum

        if (signum >= 0) != (tentative >= 0) then integerRangeEnd(signum)
        else rec(remainder.substring(1), signum, tentative)

    if s.isEmpty then 0
    else if s.charAt(0) == ' ' then solutionDan(s.substring(1))
    else if s.charAt(0) == '+' then rec(s.substring(1), 1, 0)
    else if s.charAt(0) == '-' then rec(s.substring(1), -1, 0)
    else rec(s, 1, 0)

  println(solutionG(Int.MinValue.toString))
  println(solutionDan(Int.MinValue.toString))

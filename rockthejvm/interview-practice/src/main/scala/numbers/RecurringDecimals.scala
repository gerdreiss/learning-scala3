package numbers

import scala.annotation.tailrec

extension (n: Long) def /%(d: Long): (Long, Long) = (n / d, n % d)

@tailrec
def findRecurrenceStart(
    digit: Long,
    digits: List[Long],
    remainder: Long,
    remainders: List[Long],
    currentIdx: Int = 0
): Int =
  if digits.isEmpty || remainders.isEmpty then -1
  else if digit == digits.head && remainder == remainders.head then currentIdx
  else findRecurrenceStart(digit, digits.tail, remainder, remainders.tail, currentIdx + 1)

def fractionToRecurrentDecimalsRec(
    numerator: Long,
    denominator: Long,
    digits: List[Long] = List.empty,
    remainders: List[Long] = List.empty
): String =
  val (quotient, remainder) = numerator * 10 /% denominator

  if remainder == 0 then (digits :+ quotient).mkString
  else
    val recurrenceStartIndex = findRecurrenceStart(quotient, digits, remainder, remainders)

    if recurrenceStartIndex == -1 then
      fractionToRecurrentDecimalsRec(
        remainder,
        denominator,
        digits :+ quotient,
        remainders :+ remainder
      )
    else
      val (beforeRec, recurrence) = digits.splitAt(recurrenceStartIndex)
      s"${beforeRec.mkString}(${recurrence.mkString})"

def fractionToRecurringDecimals(numerator: Int, denominator: Int): String =
  val (quotient, remainder) = numerator /% denominator

  if remainder == 0 then quotient.toString
  else s"$quotient.${fractionToRecurrentDecimalsRec(remainder, denominator)}"

object RecurringDecimals extends App:

  println(f" 1/3    =  ${(BigDecimal(1.0) / BigDecimal(3.0)).formatted("%.100f")}")
  println(f" 1/2    =  ${(BigDecimal(1.0) / BigDecimal(2.0)).formatted("%.100f")}")
  println(f" 1/6    =  ${(BigDecimal(1.0) / BigDecimal(6.0)).formatted("%.100f")}")
  println(f" 1/333  =  ${(BigDecimal(1.0) / BigDecimal(333.0)).formatted("%.100f")}")
  println(f" 1/7    =  ${(BigDecimal(1.0) / BigDecimal(7.0)).formatted("%.100f")} ")
  println(f" 1/2003 =  ${(BigDecimal(1.0) / BigDecimal(2003.0)).formatted("%.100f")}")
  println(f"-1/2    = ${(BigDecimal(-1.0) / BigDecimal(2.0)).formatted("%.100f")}")
  println("-" * 100)
  println(s" 1/3    = ${fractionToRecurringDecimals(1, 3)}")
  println(s" 1/2    = ${fractionToRecurringDecimals(1, 2)}")
  println(s" 4/2    = ${fractionToRecurringDecimals(4, 2)}")
  println(s" 1/6    = ${fractionToRecurringDecimals(1, 6)}")
  println(s" 1/333  = ${fractionToRecurringDecimals(1, 333)}")
  println(s" 1/7    = ${fractionToRecurringDecimals(1, 7)}")
  println(s" 1/2003 = ${fractionToRecurringDecimals(1, 2003)}")

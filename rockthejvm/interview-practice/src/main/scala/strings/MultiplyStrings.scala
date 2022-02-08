package strings

import scala.annotation.tailrec

object MultiplyStrings extends App:

  /**
   * My solution
   */

  extension (s: String)
    def prepend0(len: Int): String =
      if s.length == len then s else "0" * (len - s.length) + s

    def add(t: String): String =
      val maxLength       = s.length max t.length
      val (carry, result) = (s.prepend0(maxLength) zip t.prepend0(maxLength))
        .map((left, right) => left.asDigit + right.asDigit)
        .reverse
        .foldLeft((0, "")) { case ((carry, acc), sum) =>
          val result = sum + carry
          (result / 10, (result % 10) + acc)
        }
      if carry == 0 then result else s"$carry$result"

    def multiply(n: Int): String =
      if n == 0 then "0"
      else List.fill(n)(s).reduce(_ add _)

  def solutionG(a: String, b: String): String =
    b.map(_.asDigit)
      .reverse
      .zipWithIndex
      .map((d, idx) => a.multiply(d) + ("0" * idx))
      .reduce(_ add _)

  /**
   * Dan's solution
   */

  def solutionDan(a: String, b: String): String =
    def multiplyByDigit(number: List[Int], factor: Int): List[Int] =
      @tailrec
      def rec(
          remainingDigits: List[Int],
          carry: Int = 0,
          acc: List[Int] = List.empty
      ): List[Int] =
        if remainingDigits.isEmpty then
          if carry == 0 then acc.reverse
          else (carry :: acc).reverse
        else
          val newDigit   = remainingDigits.head
          val newProduct = newDigit * factor + carry
          rec(remainingDigits.tail, newProduct / 10, (newDigit % 10) :: acc)

      rec(number)

    def addTwoNumbers(as: List[Int], bs: List[Int]): List[Int] =
      def rec(
          remainingA: List[Int],
          remainingB: List[Int],
          carry: Int = 0,
          acc: List[Int] = List.empty
      ): List[Int] =
        if remainingA.isEmpty && remainingB.isEmpty then
          if carry == 0 then acc.reverse
          else (carry :: acc).reverse
        else if remainingA.isEmpty then acc.reverse ++ rec(List(carry), remainingB)
        else if remainingB.isEmpty then acc.reverse ++ rec(List(carry), remainingA)
        else
          val newSum   = remainingA.head + remainingB.head + carry
          val newDigit = newSum % 10
          val newCarry = newSum / 10
          rec(remainingA.tail, remainingB.tail, newCarry, newDigit :: acc)

      if as.isEmpty then bs
      else if bs.isEmpty then as
      else rec(as, bs)

    def multiplyDigits(as: List[Int], b: List[Int]): List[Int] =
      b.zipWithIndex
        .map { case (digit, index) =>
          List.fill(index)(0) ++ multiplyByDigit(as, digit)
        }
        .reduce(addTwoNumbers)

    val digitsA      = a.reverse.map(_ - '0').toList
    val digitsB      = b.reverse.map(_ - '0').toList
    val digitsResult = multiplyDigits(digitsA, digitsB)
    val result       = digitsResult.reverse.mkString

    // TODO check leading zeroes
    result

  println(s"Scala: ${BigInt("125137859237859327893") * BigInt("45652378957234896")}")
  println(s"G    : ${solutionG("125137859237859327893", "45652378957234896")}")
  println(s"Dan  : ${solutionDan("125137859237859327893", "45652378957234896")}")

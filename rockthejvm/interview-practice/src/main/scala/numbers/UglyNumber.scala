package numbers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object UglyNumber extends App:

  extension (n: Int)
    def isPrime: Boolean =
      n > 1 && (2 to math.sqrt(n).toInt).forall(x => n % x != 0)

  // ugly = only factors 2, 3, 5
  // assume positive number
  // examples: 6, 25, 100
  // not ugly: 14, 39
  def isUglyG(n: Int): Boolean =
    def rec(n: Int, fac: Int, acc: List[Int] = List.empty): List[Int] =
      if fac > n / 2 then acc
      else if n % fac == 0 then
        if fac.isPrime then rec(n, fac + 1, fac :: acc)
        else rec(n, fac + 1, acc ++ rec(n / fac, 2))
      else rec(n, fac + 1, acc)

    List(1, 2, 3, 5).contains(n) || !n.isPrime && rec(n, 2).distinct
      .foldLeft(true)((ugly, value) => ugly && List(2, 3, 5).contains(value))

  def nthUglyG(n: Int): Int =
    Iterator.from(1).filter(isUglyG).take(n).toList.last

  def isUglyDan(n: Int): Boolean =
    if n == 1 then true
    else if n % 2 == 0 then isUglyDan(n / 2)
    else if n % 3 == 0 then isUglyDan(n / 3)
    else if n % 5 == 0 then isUglyDan(n / 5)
    else false

  def nthUglyDan(n: Int): Int =
    def rec(idx: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int]): Int =
      val min = List(q2.head, q3.head, q5.head).min
      if idx == n then min
      else
        val newQ2 = (if min == q2.head then q2.tail else q2).enqueue(min * 2)
        val newQ3 = (if min == q3.head then q3.tail else q3).enqueue(min * 3)
        val newQ5 = (if min == q5.head then q5.tail else q5).enqueue(min * 5)

        rec(idx + 1, newQ2, newQ3, newQ5)

    if n == 1 then 1
    else rec(2, Queue(2), Queue(3), Queue(5))

  println(s"       1 ${isUglyG(1)}")
  println(s"       2 ${isUglyG(2)}")
  println(s"       3 ${isUglyG(3)}")
  println(s"       5 ${isUglyG(5)}")
  println(s"       6 ${isUglyG(6)}")
  println(s"      14 ${isUglyG(14)}")
  println(s"      25 ${isUglyG(25)}")
  println(s"      39 ${isUglyG(39)}")
  println(s"     100 ${isUglyG(100)}")
  println(s"    1200 ${isUglyG(1200)}")
  println(s"52678578 ${isUglyG(52678578)}")

  println("-" * 100)
  // (1 to 100).map(nthUglyG).foreach(println)

  println(isUglyG(7))
  println(isUglyDan(7))
  println(Iterator.from(1).filter(isUglyG).take(7).toList)
  println(Iterator.from(1).filter(isUglyDan).take(7).toList)

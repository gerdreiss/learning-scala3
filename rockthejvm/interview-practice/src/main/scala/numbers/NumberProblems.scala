package numbers

import scala.annotation.tailrec

def isPrimeDan(n: Int): Boolean =
  // Complexity: O(sqrt(N))
  @tailrec
  def rec(currentDivisor: Int): Boolean =
    if currentDivisor > math.sqrt(n) then true
    else n % currentDivisor != 0 && rec(currentDivisor + 1)

  n > 1 && rec(2)

def isPrimeG(n: Int): Boolean =
  n > 1 && (2 to math.sqrt(n).toInt).forall(x => n % x != 0)

object NumberProblems extends App:

  println(
    Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83,
      89, 97)
  )
  println((1 to 100).filter(isPrimeDan))
  println((1 to 100).filter(isPrimeG))

  println("print the first 100 of a huge list of prime numbers (G)")
  val primesG = (1 to 10000000).filter(isPrimeG)
  println(primesG.take(100))

  println("print the first 100 of a huge list of prime numbers (Dan)")
  val primesDan = (1 to 10000000).filter(isPrimeDan)
  println(primesDan.take(100))

  println("Are the lists equal?")
  println(if primesDan == primesG then "yes" else "nooooooo!")

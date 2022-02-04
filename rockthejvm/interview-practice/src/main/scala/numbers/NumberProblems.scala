package numbers

import scala.annotation.tailrec

def isPrimeG(n: Int): Boolean =
  n > 1 && (2 to math.sqrt(n).toInt).forall(x => n % x != 0)

def isPrimeDan(n: Int): Boolean =
  // Complexity: O(sqrt(N))
  @tailrec
  def rec(currentDivisor: Int): Boolean =
    if currentDivisor > math.sqrt(n) then true
    else n % currentDivisor != 0 && rec(currentDivisor + 1)

  n > 1 && rec(2)

// the prime constituents of the given number
// this method finds all distinct factors, but doesn't return the entire list
// e.g. decomposeG(16) return List(2) instead of List(2, 2, 2, 2)
def decomposeG(n: Int): List[Int] =
  (2 to math.sqrt(n).toInt).toList.filter(x => isPrimeG(x) && n % x == 0)

// Complexity: O(sqrt(N))
def decomposeDan(n: Int): List[Int] =
  @tailrec
  def rec(remainder: Int, currentDiv: Int, acc: List[Int]): List[Int] =
    if currentDiv > math.sqrt(n) then remainder :: acc
    else if remainder % currentDiv == 0 then
      rec(remainder / currentDiv, currentDiv, currentDiv :: acc)
    else rec(remainder, currentDiv + 1, acc)

  rec(n, 2, List.empty).sorted.filter(_ > 1)

extension (n: Int)
  def isPrime: Boolean =
    n > 1 && (2 to math.sqrt(n).toInt).forall(x => n % x != 0)

  def decompose: List[Int] =
    @tailrec
    def rec(remainder: Int, currentDiv: Int, acc: List[Int]): List[Int] =
      if currentDiv > math.sqrt(n) then remainder :: acc
      else if remainder % currentDiv == 0 then
        rec(remainder / currentDiv, currentDiv, currentDiv :: acc)
      else rec(remainder, currentDiv + 1, acc)

    rec(n, 2, List.empty).sorted.filter(_ > 1)

object NumberProblems extends App:

  // println(Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97))
  // println((1 to 100).filter(isPrimeDan))
  // println((1 to 100).filter(isPrimeG))

  // println("print the first 100 of a huge list of prime numbers (G)")
  // val primesG = (1 to 10000000).filter(isPrimeG)
  // println(primesG.take(100))

  // println("print the first 100 of a huge list of prime numbers (Dan)")
  // val primesDan = (1 to 10000000).filter(isPrimeDan)
  // println(primesDan.take(100))

  // println("Are the lists equal?")
  // println(if primesDan == primesG then "yes" else "nooooooo!")

  // println(decomposeG(105))
  // println(decomposeG(149089))
  // println(decomposeG(Int.MaxValue - 1))
  // println(decomposeGG(Int.MaxValue - 1))
  // println(decomposeDan(Int.MaxValue - 1))
  // println(decomposeDan(16))
  // println(decomposeG(16))

  println

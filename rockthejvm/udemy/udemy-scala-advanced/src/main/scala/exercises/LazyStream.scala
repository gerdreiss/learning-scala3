package exercises

import java.util as ju

abstract class MyStream[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]
  def #::[B >: A](b: B): MyStream[B]
  def ++[B >: A](that: => MyStream[B]): MyStream[B]
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(p: A => Boolean): MyStream[A]
  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A] = take(n).toList(List.empty)

  @annotation.tailrec
  final def toList[B >: A](acc: List[B] = List.empty): List[B] =
    if isEmpty then acc
    else tail.toList(head :: acc)

end MyStream

object EmptyStream extends MyStream[Nothing]:
  override def isEmpty: Boolean = true
  override def head: Nothing = throw ju.NoSuchElementException()
  override def tail: MyStream[Nothing] = throw ju.NoSuchElementException()
  override def #::[B >: Nothing](b: B): MyStream[B] = Cons(b, this)
  override def ++[B >: Nothing](that: => MyStream[B]): MyStream[B] = that
  override def foreach(f: Nothing => Unit): Unit = ()
  override def map[B](f: Nothing => B): MyStream[B] = this
  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  override def filter(p: Nothing => Boolean): MyStream[Nothing] = this
  override def take(n: Int): MyStream[Nothing] = this
end EmptyStream

class Cons[+A](h: A, t: => MyStream[A]) extends MyStream[A]:
  override val isEmpty: Boolean = false

  override val head: A = h

  override lazy val tail: MyStream[A] = t

  override def #::[B >: A](b: B): MyStream[B] =
    Cons(b, this)

  override def ++[B >: A](that: => MyStream[B]): MyStream[B] =
    Cons(head, tail ++ that)

  override def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)

  override def map[B](f: A => B): MyStream[B] =
    Cons(f(head), tail.map(f))

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    f(head) ++ tail.flatMap(f)

  override def filter(p: A => Boolean): MyStream[A] =
    if p(head) then Cons(head, tail.filter(p))
    else tail.filter(p)

  override def take(n: Int): MyStream[A] =
    if n <= 0 then EmptyStream
    else if n == 1 then Cons(head, EmptyStream)
    else Cons(head, tail.take(n - 1))

end Cons

object MyStream:
  def from[A](start: A)(generator: A => A): MyStream[A] =
    Cons(start, MyStream.from(generator(start))(generator))

def fib(first: BigInt, second: BigInt): MyStream[BigInt] =
  Cons(first, fib(second, first + second))

def primes(numbers: MyStream[Int]): MyStream[Int] =
  if numbers.isEmpty then numbers
  else Cons(numbers.head, primes(numbers.tail.filter(_ % numbers.head != 0)))

@main def testMyStream(): Unit =
  println("-" * 50)
  println("Fibonacci")
  println(fib(1, 1).take(10).toList())
  println("-" * 50)
  println("Primes")
  println(primes(MyStream.from(2)(_ + 1)).take(10).toList())

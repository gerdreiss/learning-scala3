// https://betterprogramming.pub/create-stack-safe-recursion-using-trampolines-in-scala-7c0ecd003fb9

import scala.annotation.tailrec

object StackSafeRecursionUsingTrampolines extends App:

  def fib(n: Int): Int =
    if n == 0 then 0
    else if n == 1 then 1
    else fib(n - 1) + fib(n - 2)

  // this results in java.lang.StackOverflowError
  // val f = fib(100000)

  sealed trait Trampoline[-A]:
    def flatMap[B, C <: A](f: C => Trampoline[B]): Trampoline[B] = FlatMap(this, f)
    def map[B, C <: A](f: C => B): Trampoline[B]                 = flatMap(f andThen (Return(_)))

  case class Return[A](a: A)                                        extends Trampoline[A]
  case class Suspense[A](a: () => Trampoline[A])                    extends Trampoline[A]
  case class FlatMap[A, B](a: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  def fibTailRec(n: Int): Trampoline[Int] =
    if n == 0 then Return[Int](0)
    else if n == 1 then Return[Int](1)
    else
      FlatMap[Int, Int](
        Suspense(() => fibTailRec(n - 1)),
        i => FlatMap[Int, Int](Suspense(() => fibTailRec(n - 2)), j => Return[Int](i + j))
      )

  // @tailrec
  // def run(trampoline: Trampoline[Int]): Int = trampoline match
  //   case Return(a: Int) => a
  //   case Suspense(x: (() => Trampoline[Int])) => run(x())
  //   case FlatMap(x: Trampoline[Int], f: (Int => Trampoline[Int])) =>
  //     x match
  //       case Return(a: Int) => run(f(a))
  //       case Suspense(a: (() => Trampoline[Int])) => run(FlatMap(a(), f))
  //       case FlatMap(x1: Trampoline[Int], f1: (Int => Trampoline[Int])) =>
  //         run(x1.flatMap((x2: Int) => f1(x2).flatMap(f): Trampoline[Int]))

  // // this doesn't result in java.lang.StackOverflowError, but it takes forever
  // println(run(fibTailRec(1000)))

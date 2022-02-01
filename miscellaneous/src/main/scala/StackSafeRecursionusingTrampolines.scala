// https://betterprogramming.pub/create-stack-safe-recursion-using-trampolines-in-scala-7c0ecd003fb9
import scala.xml.dtd.ContentModel.Translator
import scala.annotation.tailrec
object StackSafeRecursionusingTrampolines extends App:

  def fib(n: Int): Int =
    if n == 0 then 0
    else if n == 1 then 1
    else fib(n - 1) + fib(n - 2)

  // this results in java.lang.StackOverflowError
  // val f = fib(100000)

  sealed trait Trampolining[-A]:
    def flatMap[B, C <: A](f: C => Trampolining[B]): Trampolining[B] = FlatMap(this, f)
    def map[B, C <: A](f: C => B): Trampolining[B]                   = flatMap(f andThen (Return(_)))

  case class Return[A](a: A)                                            extends Trampolining[A]
  case class Suspense[A](a: () => Trampolining[A])                      extends Trampolining[A]
  case class FlatMap[A, B](a: Trampolining[A], f: A => Trampolining[B]) extends Trampolining[B]

  def fibTailRef(n: Int): Trampolining[Int] =
    if n == 0 then Return[Int](0)
    else if n == 1 then Return[Int](1)
    else
      FlatMap[Int, Int](
        Suspense(() => fibTailRef(n - 1)),
        i => FlatMap[Int, Int](Suspense(() => fibTailRef(n - 2)), j => Return[Int](i + j))
      )

  @tailrec
  def run(trampoline: Trampolining[Int]): Int = trampoline match
    case Return(a: Int)                                               => a
    case Suspense(x: (() => Trampolining[Int]))                       => run(x())
    case FlatMap(x: Trampolining[Int], f: (Int => Trampolining[Int])) =>
      x match
        case Return(a: Int)                                                 => run(f(a))
        case Suspense(a: (() => Trampolining[Int]))                         => run(FlatMap(a(), f))
        case FlatMap(x1: Trampolining[Int], f1: (Int => Trampolining[Int])) =>
          run(x1.flatMap((x2: Int) => f1(x2).flatMap(f): Trampolining[Int]))

  // this doesn't result in java.lang.StackOverflowError, but it takes forever
  println(run(fibTailRef(1000)))

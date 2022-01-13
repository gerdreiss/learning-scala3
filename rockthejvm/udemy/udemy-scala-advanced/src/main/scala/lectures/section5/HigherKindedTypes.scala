package lectures.section5

import scala.util.Try

object HigherKindedTypes:

  trait AHigherKindedType[F[_]]

  trait MyList[T]:
    def flatMap[S](f: T => MyList[S]): MyList[S]

  trait MyOpt[T]:
    def flatMap[S](f: T => MyOpt[S]): MyOpt[S]

  trait MyTry[T]:
    def flatMap[S](f: T => MyTry[S]): MyTry[S]

  // combine/multiply List(1, 2) mult List("a", "b") => List("1a", "1b", "2a", "2b")
  def combine[A, B](as: List[A], bs: List[B]): List[(A, B)] =
    for
      a <- as
      b <- bs
    yield (a, b)

  def combine[A, B](as: Option[A], bs: Option[B]): Option[(A, B)] =
    for
      a <- as
      b <- bs
    yield (a, b)

  def combine[A, B](as: Try[A], bs: Try[B]): Try[(A, B)] =
    for
      a <- as
      b <- bs
    yield (a, b)

  // use Higher Kinded Type
  trait Monad[F[_], A]:
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]

  class ListMonad[A] extends Monad[List, A]:
    override def map[B](f: A => B): List[B] = ???
    override def flatMap[B](f: A => List[B]) = ???

  def combine[F[_], A, B](using ma: Monad[F, A], mb: Monad[F, B]): F[(A, B)] =
    for
      a <- ma
      b <- mb
    yield (a, b)

@main def HigherKindedTypesTest(): Unit =
  ???

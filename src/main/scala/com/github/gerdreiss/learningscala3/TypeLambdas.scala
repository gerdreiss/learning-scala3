package com.github.gerdreiss.learningscala3

object TypeLambdas extends App:

  /*
    - Kinds - types of types
    - Int, String = value level kind (level 0) => attach to values
    - List, Option = level 1 kind ("generics")
    - Functor, Monad = level 2 kind ("generics of generics")
   */
  val aNumber: Int = 42

  trait Functor[F[_]]
  val functorOption = new Functor[Option] {}

  // List is similar to a function = type constructor
  type MyList = [T] =>> List[T]

  type MapWithStringKey = [T] =>> Map[String, T]
  type MapWithStringKey2[T] = Map[String ,T] // exactly the same

  val addressBook: MapWithStringKey[String] = Map.empty

  type SpecialEither = [T, E] =>> Either[E, Option[T]]
  val specialEither: SpecialEither[Int, String] /* Either[String, Option[Int]] */ = Right(Some(2))


  trait Monad[M[_]]:
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(transformation: A => M[B]): M[B]

  // monads for Either
  // class EitherMonad[E] extends Monad[Either[E, ?]] {}

  class EitherMonad[E] extends Monad[[T] =>> Either[E, T]]:
    def pure[A](value: A): Either[E, A] = Right(value)
    def flatMap[A, B](ma: Either[E, A])(transformation: A => Either[E, B]): Either[E, B] =
      ma match
          case Right(value) => transformation(value)
          case Left(error) => Left(error)

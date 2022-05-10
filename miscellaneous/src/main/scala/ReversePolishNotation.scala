// A Prelude of Purity: Scaling Back ZIO by Jorge Vasquez - ZIO World 2022 (https://youtu.be/Vq449igxuLk)

import java.io.IOException

object Util:
  def getExprElements(expr: String): List[String] =
    expr.split(" ") match
      case xs: Array[String] => xs.toList
      case _                 => throw new IllegalStateException("WTF???")

object UsingCatsStateA:
  import cats.data.*
  import cats.implicits.*

  type Stack = List[Int]

  type Eff[A] = State[Stack, A]

  def push(x: Int): Eff[Unit] =
    State.modify(x :: _)

  val pop: Eff[Int] =
    for
      stack <- State.get
      x     <- State.set(stack.tail).as(stack.head)
    yield x

  def evalRPN(elements: List[String]): Int =
    def processElement(element: String): Eff[Unit] =
      element match
        case "+" => processTopElements(_ + _)
        case "-" => processTopElements(_ - _)
        case "*" => processTopElements(_ * _)
        case x   => push(x.toInt)

    def processTopElements(op: (Int, Int) => Int): Eff[Unit] =
      for
        fst <- pop
        snd <- pop
        _   <- push(op(fst, snd))
      yield ()

    (elements.traverse(processElement) *> pop).runA(List.empty).value

end UsingCatsStateA

object UsingCatsStateEitherA:
  import cats.data.*
  import cats.implicits.*

  type Stack = List[Int]

  type Eff[A] = State[Stack, Either[String, A]]

  def push(x: Int): Eff[Unit] =
    State.modify[Stack](x :: _).map(_.asRight)

  val pop: Eff[Int] =
    for
      stack <- State.get
      x     <- stack match
                 case head :: tail => State.set(tail).as(head.asRight)
                 case _            => State.pure("No operands left".asLeft)
    yield x

  def evalRPN(elements: List[String]): Either[String, Int] =
    def processElements(elements: List[String]): Eff[Unit] =
      elements match
        case x :: xs =>
          for
            processed <- processElement(x)
            result    <- processed match
                           case Right(_) => processElements(xs)
                           case Left(e)  => State.pure(e.asLeft)
          yield result
        case _       => State.pure(Right(()))

    def processElement(element: String): Eff[Unit] =
      element match
        case "+" => processTopElements(_ + _)
        case "-" => processTopElements(_ - _)
        case "*" => processTopElements(_ * _)
        case x   =>
          x.toIntOption match
            case Some(x) => push(x)
            case None    => State.pure(s"Not a number: x".asLeft)

    def processTopElements(op: (Int, Int) => Int): Eff[Unit] =
      for
        fst    <- pop
        snd    <- pop
        result <- (fst, snd).mapN(op) match
                    case Right(x) => push(x)
                    case Left(e)  => State.pure(e.asLeft)
      yield result

    val state: Eff[Int] =
      for
        processed <- processElements(elements)
        result    <- processed match
                       case Right(_) => pop
                       case Left(e)  => State.pure(e.asLeft)
      yield result

    state.runA(List.empty).value

end UsingCatsStateEitherA

object UsingCatsMonadTransformers:
  import cats.data.*
  import cats.implicits.*

  type Stack = List[Int]

  type Eff[A] = EitherT[State[Stack, *], String, A]

  def push(x: Int): Eff[Unit] =
    EitherT.liftF(State.modify[Stack](x :: _))

  val pop: Eff[Int] =
    for
      stack <- EitherT.liftF(State.get)
      x     <- stack match
                 case head :: tail => EitherT.liftF(State.set(tail).as(head))
                 case _            => EitherT.leftT[State[Stack, *], Int]("No operands left")
    yield x

  def evalRPN(elements: List[String]): Either[String, Int] =
    def processElement(element: String): Eff[Unit] =
      element match
        case "+" => processTopElements(_ + _)
        case "-" => processTopElements(_ - _)
        case "*" => processTopElements(_ * _)
        case x   => EitherT.fromOption(x.toIntOption, s"Not a number: $x").flatMap(push)

    def processTopElements(op: (Int, Int) => Int): Eff[Unit] =
      for
        fst    <- pop
        snd    <- pop
        result <- push(op(fst, snd))
      yield result

    (elements.traverse(processElement) *> pop).value.runA(List.empty).value

end UsingCatsMonadTransformers

object UsingZio2:
  import zio.*

  type Stack = Ref[List[Int]]

  type Eff[+A] = ZIO[Stack, String, A]

  def push(x: Int): Eff[Unit] =
    ZIO.service[Stack].flatMap(_.update(x :: _))

  val pop: Eff[Int] =
    for
      ref   <- ZIO.service[Stack]
      stack <- ref.get
      x     <- stack match
                 case head :: tail => ref.set(tail).as(head)
                 case _            => ZIO.fail("No operands left")
    yield x

  def evalRPN(elements: List[String]): Either[String, Int] =
    def processElement(element: String): Eff[Unit] =
      element match
        case "+" => processTopElements(_ + _)
        case "-" => processTopElements(_ - _)
        case "*" => processTopElements(_ * _)
        case x   => ZIO.from(x.toIntOption).flatMap(push).orElseFail("Not a number: " + x)

    def processTopElements(op: (Int, Int) => Int): Eff[Unit] =
      for
        fst    <- pop
        snd    <- pop
        result <- push(op(fst, snd))
      yield result

    Runtime.default.unsafeRun[IOException, Either[String, Int]](
      (ZIO.foreachDiscard(elements)(processElement) *> pop).either
        .provideLayer(ZLayer.fromZIO(Ref.make(List.empty[Int])))
    )

end UsingZio2

object UsingZPure:
  import zio.prelude.*

  type Stack = List[Int]

  type Eff[+A] = EState[Stack, String, A] // ZPure[Nothing, Stack, Stack, Any, String, A]

  def push(x: Int): Eff[Unit] =
    EState.update(x :: _)

  val pop: Eff[Int] =
    for
      stack <- EState.get[Stack]
      x     <- stack match
                 case head :: tail => EState.set(tail).as(head)
                 case _            => EState.fail("No operands left")
    yield x

  def evalRPN(elements: List[String]): Either[String, Int] =
    def processElement(element: String): Eff[Unit] =
      element match
        case "+" => processTopElements(_ + _)
        case "-" => processTopElements(_ - _)
        case "*" => processTopElements(_ * _)
        case x   => EState.fromOption(x.toIntOption).flatMap(push).orElseFail(s"Not a number: $x")

    def processTopElements(op: (Int, Int) => Int): Eff[Unit] =
      for
        fst    <- pop
        snd    <- pop
        result <- push(op(fst, snd))
      yield result

    (elements.forEach(processElement) *> pop).provideState(List.empty).runEither

end UsingZPure

@main def main(): Unit =
  val elements = Util.getExprElements("1 2 + 10 * 42 -")
  println(UsingZPure.evalRPN(elements))

import zio.*
import zio.prelude.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

import Assertion.*

object MovingAvgZIO:

  object Numbers extends Subtype[List[Double]]:
    override inline def assertion = Assertion.notEqualTo(List.empty)

  object Window extends Subtype[Int]:
    override inline def assertion = Assertion.greaterThan(0)

  type Numbers = Numbers.Type
  type Window  = Window.Type

  final class Input private (val numbers: Numbers, val window: Window)
  object Input:
    def make(numbers: List[Double], window: Int): Either[List[String], Input] =
      Validation
        .validateWith(Numbers.make(numbers), Window.make(window))(Input(_, _))
        .flatMap(input =>
          Validation.fromEither(
            Either.cond(
              input.window <= input.numbers.length,
              input,
              "Window must be smaller than or equal to the length of the numbers list"
            )
          )
        )
        .toEither
        .leftMap(_.toList)

  def compute(input: Input): List[Double] =
    @tailrec
    def recurse(
        remainingInput: List[Double],
        movingSum: Double,
        acc: Queue[Double] = Queue.empty
    ): List[Double] =
      if remainingInput.length == input.window then (acc :+ movingSum / input.window).toList
      else
        recurse(
          remainingInput.tail,
          movingSum - remainingInput.head + remainingInput(input.window),
          acc :+ movingSum / input.window
        )

    recurse(input.numbers, input.numbers.take(input.window).sum)

  end compute

@main def calculateMovingAvgWithZioPrelude(): Unit =
  import MovingAvgZIO.*

  val rnd = new Random

  val window  = rnd.nextInt(10)
  val numbers = (1 to window * window + window / 3).map(_ => rnd.nextDouble).toList
  Input.make(numbers, 0) match
    case Left(errors) => errors.foreach(println)
    case Right(input) => println(compute(input))

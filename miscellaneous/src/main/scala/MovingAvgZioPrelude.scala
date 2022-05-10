import zio.prelude.*

import scala.annotation.tailrec
import scala.util.Random

import Assertion.*

object MovingAvgZioPrelude:

  /** it's probably best to use zio.NonEmptyChunk or zio.prelude.NonEmptyList instead, but here we are... */
  object Numbers extends Subtype[List[Double]]:
    override inline def assertion = Assertion.notEqualTo(List.empty)

  object Window extends Subtype[Int]:
    override inline def assertion = Assertion.greaterThan(0)

  type Numbers = Numbers.Type
  type Window  = Window.Type

  sealed abstract case class Input private (val numbers: Numbers, val window: Window)
  object Input:
    def make(numbers: List[Double], window: Int): Either[List[String], Input] =
      Validation
        .validateWith(Numbers.make(numbers), Window.make(window))(new Input(_, _) {})
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
        acc: Vector[Double] = Vector.empty
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

end MovingAvgZioPrelude

@main def calculateMovingAvgWithZioPrelude(): Unit =
  import MovingAvgZioPrelude.*

  val rnd = new Random

  val window  = rnd.between(1, 10)
  val numbers = (1 to window * window + window / 3).map(_ => rnd.between(0.001, 10.0)).toList
  Input.make(numbers, window) match
    case Left(errors) => errors.foreach(println)
    case Right(input) => println(compute(input))

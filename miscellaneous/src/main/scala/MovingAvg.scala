import cats.data.*
import cats.data.Validated.*
import cats.implicits.*

import scala.annotation.tailrec
import scala.compiletime.ops.int
import scala.util.Random

object MovingAvg:

  type ErrorsOr[A]  = Either[List[InputError], A]
  type InvalidOr[A] = ValidatedNec[InputError, A]

  enum InputError:
    case EmptyInput
    case ZeroOrNegativeWindow(w: Int)
    case TooLargeWindow(window: Int, inputSize: Int)

  final class Input private (val numbers: List[Double], val window: Int):
    override def toString: String =
      s"Input { numbers = ${numbers.mkString("[", ",", "]")}, window = $window }"

  object Input:
    import InputError.*

    def create(numbers: List[Double], window: Int): ErrorsOr[Input] =
      (validateNumbers(numbers), validateWindow(window))
        .mapN(Input(_, _))
        .andThen(validateInput)
        .toEither
        .left
        .map(_.toList)

    private def validateNumbers(numbers: List[Double]): InvalidOr[List[Double]] =
      Validated.condNec(numbers.nonEmpty, numbers, EmptyInput)

    private def validateWindow(window: Int): InvalidOr[Int] =
      Validated.condNec(window > 0, window, ZeroOrNegativeWindow(window))

    private def validateInput(input: Input): InvalidOr[Input] =
      Validated
        .condNec(
          input.window <= input.numbers.length,
          input,
          TooLargeWindow(input.window, input.numbers.length)
        )

  end Input

  def createInput(input: List[Double], window: Int): ErrorsOr[Input] =
    Input.create(input, window)

  def compute(input: Input): List[Double] =
    @tailrec
    def recurse(
        remainingInput: List[Double],
        movingSum: Double,
        acc: List[Double] = List.empty
    ): List[Double] =
      if remainingInput.length == input.window then movingSum / input.window :: acc
      else
        recurse(
          remainingInput.tail,
          movingSum - remainingInput.head + remainingInput(input.window),
          movingSum / input.window :: acc
        )

    recurse(input.numbers, input.numbers.take(input.window).sum).reverse

  end compute

end MovingAvg

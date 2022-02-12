import cats.data.*
import cats.data.Validated.*
import cats.implicits.*

import scala.annotation.tailrec
import scala.compiletime.ops.int
import scala.util.Random

enum MovingAvgInputError:
  case EmptyInput
  case ZeroOrNegativeWindow(w: Int)
  case TooLargeWindow(window: Int, inputSize: Int)

final class MovingAvgInput private (val numbers: List[Double], val window: Int):
  override def toString: String =
    s"MovingAvgInput { numbers = ${numbers.mkString("[", ",", "]")}, window = $window }"

object MovingAvgInput:
  import MovingAvgInputError.*

  def create(
      input: List[Double],
      window: Int
  ): Either[List[MovingAvgInputError], MovingAvgInput] =
    (validateInput(input), validateWindow(window))
      .mapN(MovingAvgInput(_, _))
      .andThen(validateMovingAvgInput)
      .toEither
      .left
      .map(_.toList)

  private def validateInput(
      input: List[Double]
  ): ValidatedNec[MovingAvgInputError, List[Double]] =
    Validated.condNec(input.nonEmpty, input, EmptyInput)

  private def validateWindow(window: Int): ValidatedNec[MovingAvgInputError, Int] =
    Validated.condNec(window > 0, window, ZeroOrNegativeWindow(window))

  private def validateMovingAvgInput(
      input: MovingAvgInput
  ): ValidatedNec[MovingAvgInputError, MovingAvgInput] =
    Validated
      .condNec(
        input.numbers.take(input.window).length == input.window,
        input,
        TooLargeWindow(input.window, input.numbers.length)
      )

end MovingAvgInput

def computeMovingAvg(input: MovingAvgInput): List[Double] =
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

end computeMovingAvg

object MovingAvgApp extends App:

  val r = new Random

  val input  = (1 to r.nextInt(10000) by r.nextInt(1000)).map(_.toDouble).toList
  val window = r.nextInt(50)

  println(MovingAvgInput.create(List.empty, -1))
  println(MovingAvgInput.create(input, window))

  println(input.sliding(window).map(_.sum / window).toList)
  println(MovingAvgInput.create(input, window).map(computeMovingAvg))

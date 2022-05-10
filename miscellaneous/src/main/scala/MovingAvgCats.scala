import cats.data.*
import cats.data.Validated.*
import cats.implicits.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

object MovingAvgCats:

  type ErrorsOr[A]  = Either[List[InputError], A]
  type InvalidOr[A] = ValidatedNec[InputError, A]

  enum InputError:
    case EmptyInput
    case ZeroOrNegativeWindow(w: Int)
    case TooLargeWindow(window: Int, inputSize: Int)

  final class Input private (val numbers: List[Double], val window: Int):
    override def toString: String =
      s"Input { window = $window, numbers = ${numbers.mkString("[", ",", "]")} }"

  object Input:
    import InputError.*

    def make(numbers: List[Double], window: Int): ErrorsOr[Input] =
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
    Input.make(input, window)

  def compute(input: Input): List[Double] =
    @tailrec
    def recurse(
        remainingInput: List[Double],
        movingSum: Double,
        acc: Queue[Double] = Queue.empty
    ): Queue[Double] =
      if remainingInput.length == input.window then acc :+ movingSum / input.window
      else
        recurse(
          remainingInput.tail,
          movingSum - remainingInput.head + remainingInput(input.window),
          acc :+ movingSum / input.window
        )

    recurse(input.numbers, input.numbers.take(input.window).sum).toList

  end compute

end MovingAvgCats

@main def calculateMovingAvg(): Unit =
  import MovingAvgCats.*

  val rnd = new Random

  val window  = rnd.nextInt(10)
  val numbers = (1 to window * window + window / 3).map(_ => rnd.nextDouble).toList

  Input.make(numbers, window) match
    case Left(errors) => errors.foreach(println)
    case Right(input) => println(compute(input))

package interview

import scala.annotation.tailrec

object MovignAvg extends App:

  enum MovingAvgError:
    case ZeroOrNegativeWindow(w: Int)
    case TooLargeWindow(size: Int)

  def computeMovingAvg(input: List[Double], window: Int): Either[MovingAvgError, List[Double]] =
    @tailrec
    def recurse(
        remainingInput: List[Double],
        movingSum: Double,
        acc: List[Double] = List.empty
    ): List[Double] =
      if remainingInput.length == window then movingSum / window :: acc
      else
        recurse(
          remainingInput.tail,
          movingSum - remainingInput.head + remainingInput(window),
          movingSum / window :: acc
        )

    if window < 1 then Left(MovingAvgError.ZeroOrNegativeWindow(window))
    else if window > input.length then Left(MovingAvgError.TooLargeWindow(input.length))
    else Right(recurse(input, input.take(window).sum).reverse)

  end computeMovingAvg

  val input  = (1 to 51 by 3).map(_.toDouble).toList
  val window = 9

  println(input.sliding(window).map(_.sum / window).toList)
  println(computeMovingAvg(input, window))

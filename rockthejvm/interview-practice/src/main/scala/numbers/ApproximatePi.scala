package numbers

import scala.util.Random

def approximatePi(points: Int): Double =
  val pointsInsideCircle = (1 to points)
    .map { _ =>
      val x = Random.nextDouble()
      val y = Random.nextDouble()
      x * x + y * y
    }
    .count(distance => distance < 1)

  pointsInsideCircle * 4.0 / points // approximation of pi

object ApproximatePi extends App:
  println(s"Reference: ${Math.PI}")
  println(approximatePi(1000))
  println(approximatePi(10000))
  println(approximatePi(100000))
  println(approximatePi(1000000))
  println(approximatePi(10000000))
  println(approximatePi(100000000))
  // OutOfMemoryError: println(approximatePi(1000000000))

package lectures.section4

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{ Failure, Success }

def calculateMeaningOfLife: Int =
  Thread.sleep(2000)
  42

val aFuture = Future {
  calculateMeaningOfLife // this runs on another thread
}

@main def futuresAndPromises(): Unit =
  aFuture.onComplete {
    case Success(meaningOfLife) => println(meaningOfLife)
    case Failure(e) => println(s"error: $e")
  }

  Thread.sleep(3000)

package tutorials.fs2

import cats.effect.*
import cats.syntax.flatMap.*
import fs2.*

import scala.concurrent.duration.*

object Model:
  case class Actor(id: Int, firstName: String, lastName: String)

object Data:
  import Model.*

  // Justice League
  val henryCavil: Actor = Actor(0, "Henry", "Cavill")
  val galGodot: Actor   = Actor(1, "Gal", "Godot")
  val ezraMiller: Actor = Actor(2, "Ezra", "Miller")
  val benFisher: Actor  = Actor(3, "Ben", "Fisher")
  val rayHardy: Actor   = Actor(4, "Ray", "Hardy")
  val jasonMomoa: Actor = Actor(5, "Jason", "Momoa")

  // Avengers
  val scarlettJohansson: Actor = Actor(6, "Scarlett", "Johansson")
  val robertDowneyJr: Actor    = Actor(7, "Robert", "Downey Jr.")
  val chrisEvans: Actor        = Actor(8, "Chris", "Evans")
  val markRuffalo: Actor       = Actor(9, "Mark", "Ruffalo")
  val chrisHemsworth: Actor    = Actor(10, "Chris", "Hemsworth")
  val jeremyRenner: Actor      = Actor(11, "Jeremy", "Renner")
  val tomHolland: Actor        = Actor(13, "Tom", "Holland")
  val tobeyMaguire: Actor      = Actor(14, "Tobey", "Maguire")
  val andrewGarfield: Actor    = Actor(15, "Andrew", "Garfield")

extension [A](ioa: IO[A])
  def debug: IO[A] =
    ioa.map { a =>
      println(s"[${Thread.currentThread.getName}] $a")
      a
    }

object Fs2Demo extends IOApp.Simple:
  import Model.*
  import Data.*

  // streams = abstractions to manage an unbounded amount of data
  // pure streams = store actual data
  val jlActors: Stream[Pure, Actor] = Stream(
    henryCavil,
    galGodot,
    ezraMiller,
    benFisher,
    rayHardy,
    jasonMomoa
  )

  // chunks
  val avngrsActors: Stream[Pure, Actor] = Stream.chunk(
    Chunk.array(
      Array(
        scarlettJohansson,
        robertDowneyJr,
        chrisEvans,
        markRuffalo,
        chrisHemsworth,
        jeremyRenner,
        tomHolland,
        tobeyMaguire,
        andrewGarfield
      )
    )
  )

  val tomHollandStream: Stream[Pure, Actor] = Stream.emit(tomHolland)
  val spiderMen: Stream[Pure, Actor]        = Stream.emits(List(tomHolland, andrewGarfield, tobeyMaguire))

  // convert a stream to a std lib data structure
  val jlActorList = jlActors.toList

  // infinite streams
  val infiniteJlActors: Stream[Pure, Actor] = jlActors.repeat
  val repeatedJlActorsList: List[Actor]     = infiniteJlActors.take(100).toList

  // effectful streams
  val savingTomHolland: Stream[IO, Actor] = Stream.eval {
    IO.println("Saving actor Tom Holland into the DB") *> IO.sleep(1.second) *> IO(tomHolland)
  }

  // compile
  val compiledStream: IO[Unit] = savingTomHolland.compile.drain

  // transformations
  val allSuperheroes = jlActors ++ avngrsActors

  // flatMap
  val printedJlActors: Stream[IO, Unit] = jlActors.flatMap { actor =>
    // perform an IO[Uint]
    Stream.eval(IO.println(actor))
  }

  // flatMap + eval = evalMap
  val printlnActors = allSuperheroes.evalMap(IO.println)

  // flatMap + eval keeping the original type = evalTap
  val tappedActors = allSuperheroes.evalTap(IO.println)

  override def run: IO[Unit] = compiledStream.debug

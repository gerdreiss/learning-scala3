package lectures.section4

import scala.util.Random
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure

case class Profile(id: String, name: String):
  def poke(p: Profile) = println(s"${this.name} poking ${p.name}")

object SocialNetwork:
  // "database"
  val names = Map(
    "fb.id.1-zuck" -> "Mark",
    "fb.id.2-bill" -> "Bill",
    "fb.id.0-dummy" -> "Dummy"
  )
  val friends = Map(
    "fb.id.1-zuck" -> "fb.id.2-bill"
  )

  val random = Random()

  // API
  def fetchProfile(id: String): Future[Profile] = Future {
    Thread.sleep(random.nextInt(300))
    Profile(id, names(id))
  }

  def fetchBFF(p: Profile): Future[Profile] = Future {
    Thread.sleep(random.nextInt(400))
    val bff = friends(p.id)
    Profile(bff, names(bff))
  }

@main def MiniSocialNetwork(): Unit =
  import SocialNetwork.*

  val result = for
    mark <- fetchProfile("fb.id.1-zuck").fallbackTo(fetchProfile("fb.id.0-dummy"))
    bill <- fetchBFF(mark).fallbackTo(fetchProfile("fb.id.0-dummy"))
  yield mark.poke(bill)

  Thread.sleep(1000)

  result.onComplete {
    case Success(()) => println("poking successful")
    case Failure(ex) => ex.printStackTrace()
  }

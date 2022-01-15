package lectures.section4

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.*

case class User(name: String)
case class Transaction(sender: String, receiver: String, amount: Double, status: String)

def fetchUser(name: String): Future[User] = Future {
// simulate fetching from DB
  Thread.sleep(500)
  User(name)
}

def createTransaction(user: User, merchant: String, amount: Double): Future[Transaction] = Future {
  // simulate some processes
  Thread.sleep(1000)
  Transaction(user.name, merchant, amount, "SUCCESS")
}

def purchase(username: String, item: String, merchant: String, cost: Double): String =
  // fetch the user from DB
  // create a transaction
  // WAIT for the transaciton to finish
  val futureStatus = for
    user <- fetchUser(username)
    trx <- createTransaction(user, merchant, cost)
  yield trx.status

  Await.result(futureStatus, 2.seconds)

@main def BankingApp(): Unit =
  val name = "Rock the JVM banking"
  println(purchase("Daniel", "Car", "CarHandler", 100000))

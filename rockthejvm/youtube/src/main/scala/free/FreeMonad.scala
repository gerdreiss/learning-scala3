package free

import cats.effect.IO

import scala.collection.mutable

// pure(a) => M[A]
// flatMap(A => M[B]) => M[B]

// classic Monad
trait Monad[F[_]]:
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

object Monad:
  def apply[F[_]](using M: Monad[F]): Monad[F] = M

// free Monad

// natural transformation
trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

trait Free[F[_], A]:
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free.FlatMap(this, f)
  def map[B](f: A => B): Free[F, B]              = flatMap(a => Free.pure(f(a)))
  def foldMap[G[_]: Monad](trf: F ~> G): G[A]    =
    this match
      case Free.Pure(a)        => Monad[G].pure(a)
      case Free.FlatMap(fa, f) => Monad[G].flatMap(fa.foldMap(trf))(a => f(a).foldMap(trf))
      case Free.Suspend(fa)    => trf(fa)

object Free:
  def pure[F[_], A](a: A): Free[F, A]      = Pure(a)
  def liftF[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)

  case class Pure[F[_], A](a: A)                                     extends Free[F, A]
  case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]
  case class Suspend[F[_], A](fa: F[A])                              extends Free[F, A]

// sequence computations as data structures,
// THEN attach the monadic type at the end

sealed trait DBOps[A]
case class Create[A](key: String, value: A) extends DBOps[Unit]
case class Read[A](key: String)             extends DBOps[Option[A]]
case class Update[A](key: String, value: A) extends DBOps[A]
case class Delete(key: String)              extends DBOps[Unit]

// definitions
type DBMonad[A] = Free[DBOps, A]

// "smart" constructors

def create[A](key: String, value: A): DBMonad[Unit] = Free.liftF(Create(key, value))
def get[A](key: String): DBMonad[Option[A]]         = Free.liftF(Read(key))
def update[A](key: String, value: A): DBMonad[A]    = Free.liftF(Update(key, value))
def delete(key: String): DBMonad[Unit]              = Free.liftF(Delete(key))

// pure computation description
def program: DBMonad[Unit] =
  for
    _ <- create("foo", "bar")
    v <- get[String]("foo")
    _ <- create("FOO", v.fold("")(_.toUpperCase))
    _ <- delete("foo")
  yield ()

// evaluate the program - interpreter/"compiler"
// we're going to use cats.effect.IO

// natural transformation DBOps -> IO
val interpreter: DBOps ~> IO =
  new:
    private val db: mutable.Map[String, String] = mutable.Map.empty

    private def serialize[A](a: A): String   = a.toString
    private def deserialize[A](s: String): A = s.asInstanceOf[A]

    def apply[A](fa: DBOps[A]): IO[A] =
      fa match
        case Create(key, value) =>
          IO(db.put(key, serialize(value))) *>
            IO.println(s"create $key $value")
        case Read(key)          =>
          IO(db.get(key)).map(_.map(deserialize).orNull) <*
            IO.println(s"read $key")
        case Update(key, value) =>
          IO(db.update(key, serialize(value))) *>
            IO(value) <*
            IO.println(s"update $key $value")
        case Delete(key)        =>
          IO(db.remove(key)) *>
            IO.println(s"delete $key")

given Monad[IO] with
  def pure[A](a: A): IO[A] = IO(a)

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

import cats.effect.unsafe.implicits.global

@main def main(): Unit =
  program.foldMap(interpreter).unsafeRunSync()

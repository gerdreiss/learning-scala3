package tutorials.http4s

import cats.*
import cats.effect.*
import cats.implicits.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.headers.*
import org.http4s.implicits.*
import org.http4s.server.*

import java.time.Year
import scala.collection.mutable
import scala.util.Try
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import java.util.UUID
import org.http4s.blaze.server.BlazeServerBuilder

object Tutorial extends IOApp:

  case class Director(firstName: String, lastName: String):
    override def toString: String = s"$firstName $lastName"

  case class DirectorDetails(firstName: String, lastName: String, genre: String)

  case class Actor(firstName: String, lastName: String):
    override def toString: String = s"$firstName $lastName"

  case class Movie(
      id: String,
      title: String,
      year: Int,
      actors: List[Actor],
      directors: List[Director]
  )

  val directors = mutable.Map(
    Director("Zack", "Snyder") -> DirectorDetails("Zack", "Snyder", "superhero")
  )

  val snjl: Movie = Movie(
    "6bcbca1e-efd3-411d-9f7c-14b872444fce",
    "Zack Snyder's Justice League",
    2021,
    List(
      Actor("Henry", "Cavill"),
      Actor("Gal", "Godot"),
      Actor("Ezra", "Miller"),
      Actor("Ben", "Affleck"),
      Actor("Ray", "Fisher"),
      Actor("Jason", "Momoa")
    ),
    List(Director("Zack", "Snyder"))
  )

  val movies: Map[String, Movie] = Map(snjl.id -> snjl)

  private def findMovieById(movieId: UUID) =
    movies.get(movieId.toString)

  private def findMoviesByDirector(director: String): List[Movie] =
    movies.values.filter(_.directors.exists(_.toString == director)).toList

  private def findMoviesByYear(year: Year): List[Movie] =
    movies.values.filter(_.year == year.getValue).toList

  private def findMoviesByDirectorAndYear(director: String, year: Year): List[Movie] =
    movies.values.filter(movie => movie.year == year.getValue && movie.directors.exists(_.toString == director)).toList

  given QueryParamDecoder[Year] = QueryParamDecoder[Int].emap { y =>
    Try(Year.of(y)).toEither
      .leftMap { e =>
        ParseFailure(e.getMessage, e.getStackTrace.mkString("\n"))
      }
  }

  object DirectoryQueryParamMatcher extends QueryParamDecoderMatcher[String]("director")
  object YearQueryParamMatcher      extends OptionalValidatingQueryParamDecoderMatcher[Year]("year")

  object DirectorPath:
    def unapply(s: String): Option[Director] =
      Try {
        val tokens = s.split(" ")
        Director(tokens(0), tokens(1))
      }.toOption

  /**
   * Endpoints: 
   *
   * - GET all movies for a director under a given year
   * - GET all actors for a movie
   * - POST add a new director
   */

  def movieRoutes[F[_]: Monad]: HttpRoutes[F] =
    val dsl = Http4sDsl[F]
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root =>
        Ok("Server is runnin...")

      case GET -> Root / "movies" :? DirectoryQueryParamMatcher(director) +& YearQueryParamMatcher(maybeYear) =>
        maybeYear match
          case None                  => Ok(findMoviesByDirector(director).asJson)
          case Some(Valid(year))     => Ok(findMoviesByDirectorAndYear(director, year).asJson)
          case Some(Invalid(errors)) => BadRequest("Invalid year value")

      case GET -> Root / "movies" / UUIDVar(movieId) / "actors" =>
        findMovieById(movieId) match
          case Some(movie) => Ok(movie.actors.asJson)
          case None        => NotFound("Movie not found")
    }

  def directorRoutes[F[_]: Monad]: HttpRoutes[F] =
    val dsl = Http4sDsl[F]
    import dsl.*

    HttpRoutes.of[F] { //
      case GET -> Root / "directors" / DirectorPath(director) =>
        directors.get(director) match
          case Some(details) => Ok(details.asJson)
          case None          => NotFound(s"No director $director found.")
    }

  def allRoutes[F[_]: Monad]: HttpRoutes[F] =
    movieRoutes[F] <+> directorRoutes[F] // cats.syntax.semigroupk.*

  def allRoutesComplete[F[_]: Monad]: HttpApp[F] =
    allRoutes[F].orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    val endpoints = Router(
      "/api"       -> movieRoutes[IO],
      "/api/admin" -> directorRoutes[IO]
    ).orNotFound

    BlazeServerBuilder[IO](runtime.compute)
      .bindHttp(8080, "localhost")
      .withHttpApp(endpoints)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)

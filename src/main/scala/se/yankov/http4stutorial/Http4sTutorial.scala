package se.yankov.http4stutorial

import cats.*
import cats.data.{Kleisli, OptionT}
import cats.effect.*
import cats.implicits.*
import cats.syntax.all.*
import com.comcast.ip4s.{ipv4, port}
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.dsl.io.*
import org.http4s.headers.*
import org.http4s.circe.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.Year
import java.util.UUID
import scala.collection.mutable
import scala.util.Try

object Http4sTutorial extends IOApp:

  opaque type Actor = String

  object Actor:
    def apply(value: String): Actor = value

  extension (actor: Actor) def value: String = actor

  final case class Movie(id: UUID, title: String, year: Int, actors: List[Actor], director: Director)

  val exampleDirector: Director = Director("Zack", "Snyder")

  val exampleMovie: Movie = Movie(
    new UUID(0,0), "Some title", 2023, "Tom Tom" :: Nil, exampleDirector
  )

  val exampleMovie2: Movie = Movie(
    new UUID(0, 1), "Some title 2", 2024, "Bill Bill" :: Nil, exampleDirector
  )

  final case class Director(firstname: String, lastname: String):
    override def toString: String = s"$firstname $lastname"

  final case class DirectorDetails(firstname: String, lastname: String, genre: String)

  val yearQueryParamsDecoder: QueryParamDecoder[Year] =
    QueryParamDecoder[Int].emap(v => Try { Year.of(v) }.toEither.left.map(e => ParseFailure(e.getMessage, e.getMessage)))

  object DirectorQueryParamMatcher extends QueryParamDecoderMatcher[String]("director")
  object YearQueryParamMatcher extends OptionalValidatingQueryParamDecoderMatcher[Year]("year")(using yearQueryParamsDecoder)

  def movieRoutes[F[_]: Async]: AppRoutes[F] = {
    val dsl: Http4sDsl[F] = Http4sDsl[F]
    import dsl.*

    AppRoutes {
      case POST -> Root / "movies" =>
        Kleisli
          .ask[F, HasMoviesDb[F]]
          .flatMapF { (env: HasMoviesDb[F]) =>
            env.getMoviesDb.update(_.updated(exampleMovie2.id, exampleMovie2)) >> Ok(exampleMovie2.asJson)
          }
      case GET -> Root / "movies" :? DirectorQueryParamMatcher(director) +& YearQueryParamMatcher(maybeYear) =>
        Kleisli
          .ask[F, HasMoviesDb[F]]
          .flatMapF { (env: HasMoviesDb[F]) =>
            maybeYear match
              case Some(validatedYear) => validatedYear.fold(
                _ => BadRequest(s"Invalid year"),
                year =>
                  env.getMoviesDb.get.flatMap { mdb =>
                    val res = mdb.values.filter(m => m.director.toString == director && m.year == year.getValue).toList
                    Ok(res.asJson)
                  }
              )
              case None =>
                env.getMoviesDb.get.flatMap { (m: Map[UUID, Movie]) =>
                  val res = m.values.filter(_.director.toString == director).toList
                  Ok(res.asJson)
                }
          }
      case GET -> Root / "movies" / UUIDVar(movieId) / "actors" =>
        Kleisli
          .ask[F, HasMoviesDb[F]]
          .flatMapF { (env: HasMoviesDb[F]) =>
            env.getMoviesDb.get.flatMap { (m: Map[UUID, Movie]) =>
              m.get(movieId).map(_.actors).fold(NotFound(s"No movie with id $movieId found"))(actors => Ok(actors.asJson))
            }
          }
    }
  }

  object DirectorPath:
    def unapply(value: String): Option[Director] =
      val parts: Array[String] = value.split(' ')
      if parts.length == 2 then Director(parts(0), parts(1)).some else None

  def directorRoutes[F[_]: Async]: AppRoutes[F] = {
    val dsl: Http4sDsl[F] = Http4sDsl[F]
    import dsl.*

    AppRoutes {
      case GET -> Root / "directors" / DirectorPath(director) =>
        Kleisli
          .ask[F, HasDirectorsDb[F]]
          .flatMapF { (env: HasDirectorsDb[F]) =>
            env.getDirectorsDb.get.flatMap { (d: Map[Director, DirectorDetails]) =>
              d.get(director).fold(NotFound(s"No director $director found"))(d => Ok(d.asJson))
            }
          }
    }
  }

  // Dependency Injection's environment
  trait Env[F[_]: Concurrent] extends HasMoviesDb[F] with HasDirectorsDb[F]

  trait HasMoviesDb[F[_] : Concurrent]:
    def getMoviesDb: Ref[F, Map[UUID, Movie]]

  trait HasDirectorsDb[F[_] : Concurrent]:
    def getDirectorsDb: Ref[F, Map[Director, DirectorDetails]]

  final case class EnvImplementation[F[_]: Concurrent](moviesDb: Ref[F, Map[UUID, Movie]], directorsDb: Ref[F, Map[Director, DirectorDetails]]) extends Env[F]:
    override def getMoviesDb: Ref[F, Map[UUID, Movie]] = moviesDb
    override def getDirectorsDb: Ref[F, Map[Director, DirectorDetails]] = directorsDb

  // ReaderT IO Env A
  type RIO[F[_], A] = Kleisli[F, Env[F], A]
  type AppRoutes[F[_]] = Kleisli[[X] =>> OptionT[F, X], Request[F], RIO[F, Response[F]]]

  def AppRoutes[F[_]: Async](pf: PartialFunction[Request[F], RIO[F, Response[F]]]): AppRoutes[F] =
    val m: Async[F] = summon[Async[F]]
    Kleisli((req: Request[F]) => OptionT(m.delay(pf.lift(req))))

  override def run(args: List[String]): IO[ExitCode] = {
    // Construct the environment (DI layers)
    val deps: Resource[IO, Env[IO]] = Resource.make[IO, Env[IO]] {
      for {
        m <- IO.ref[Map[UUID, Movie]](Map(exampleMovie.id -> exampleMovie))
        d <- IO.ref[Map[Director, DirectorDetails]](
          Map(exampleDirector -> DirectorDetails(exampleDirector.firstname, exampleDirector.lastname, "Comedy"))
        )
      } yield EnvImplementation(m, d)
    }(env => IO.println("Finalizing the deps"))
    deps.use { (env: Env[IO]) =>

      // Construct the API and thread in the environment via RIO
      val apis: AppRoutes[IO] = movieRoutes[IO] <+> directorRoutes[IO]
      val httpApp: HttpRoutes[IO] = apis.flatMapF((app: RIO[IO, Response[IO]]) => OptionT.liftF(app.run(env)))

      given LoggerFactory[IO] = Slf4jFactory.create[IO]

      EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(httpApp.orNotFound)
        .build
        .use((server: org.http4s.server.Server) => IO.never)
        .as(ExitCode.Success)
    }
  }

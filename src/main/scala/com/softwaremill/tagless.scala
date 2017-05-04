package com.softwaremill

import java.util.UUID

import cats.data.{Coproduct, EitherT, OptionT}
import cats.free.{Free, Inject}
import cats.{Functor, Id, Monad, ~>}
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object CompareTagless {
  import eitherMonad._
  
  case class User(id: UUID, name: String)
  object User {
    def parse(name: String, rawUUID: String): Either[String, User] = Try(UUID.fromString(rawUUID)) match {
      case Success(uuid) => Right(User(uuid, name))
      case Failure(t) => Left(t.getMessage)
    }
  }
  case class Destination(where: String) extends AnyVal

  trait CorporationAlg[F[_]] {
    def findUser(name: String): F[Option[User]]
    def vacationAllowance(id: UUID): F[Option[Int]]
    def sendForVacation(id: UUID, howLong: Int): F[Destination]
  }

  //

  case class ErrorCode(code: Int) extends AnyVal

  trait HttpAlg[F[_]] {
    def get(path: String): F[Either[ErrorCode, String]]
    def post(path: String, body: String): F[Either[ErrorCode, Unit]]
  }
  
  //

  def vacationProgram[F[_]: Monad](ca: CorporationAlg[F]): OptionT[F, Destination] = for {
    u <- OptionT(ca.findUser("adam"))
    v <- OptionT(ca.vacationAllowance(u.id))
    d <- OptionT.liftF(ca.sendForVacation(u.id, v))
  } yield d

  trait CorporationToHttpInterpreter[F[_]] extends CorporationAlg[EitherT[F, String, ?]] {
    def ha: HttpAlg[F]
    implicit def fm: Monad[F]

    override def findUser(name: String): EitherT[F, String, Option[User]] = {
      EitherT(ha.get(s"/api/find_user?name=$name").map {
        case Right(response) => User.parse(name, response).map(Some(_))
        case Left(ErrorCode(404)) => Right(None)
        case Left(ErrorCode(c)) => Left(s"HTTP error code: $c")
      })
    }

    override def vacationAllowance(id: UUID): EitherT[F, String, Option[Int]] = {
      val allowanceFromApi: EitherT[F, ErrorCode, String] = for {
        _ <- EitherT(ha.post(s"/api/update_allowance?id=$id", s"timestamp=${System.currentTimeMillis()}"))
        n <- EitherT(ha.get(s"/api/get_allowance?id=$id"))
      } yield n

      allowanceFromApi
        .subflatMap(n => Try(n.toInt).fold(t => Left(t.getMessage), Right(_)))
        .map(Option(_))
        .recover { case ErrorCode(404) => None }
        .leftMap { case ErrorCode(c) => s"HTTP error code: $c" }
    }

    override def sendForVacation(id: UUID, howLong: Int): EitherT[F, String, Destination] = {
      EitherT(ha.post(s"/api?send_for_vacation?id=$id", s"how_long=$howLong"))
        .map(_ => Destination("Hawaii"))
        .leftMap { case ErrorCode(c) => s"HTTP error code: $c" }
    }
  }

  trait HttpToIdInterpreter extends HttpAlg[Id] {
    override def get(path: String): Id[Either[ErrorCode, String]] = Left(ErrorCode(404))

    override def post(path: String, body: String): Id[Either[ErrorCode, Unit]] =
      Left(ErrorCode(404))
  }

  def main(args: Array[String]): Unit = {
    println(
      vacationProgram[EitherT[Id, String, ?]](new CorporationToHttpInterpreter[Id] {
        override implicit def fm: Monad[Id] = cats.catsInstancesForId
        override def ha: HttpAlg[Id] = new HttpToIdInterpreter {}
      })(catsDataMonadErrorForEitherT(implicitly[Monad[Id]]))
    )
  }
}
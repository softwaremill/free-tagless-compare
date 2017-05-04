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

object CompareFree {
  import eitherMonad._

  case class User(id: UUID, name: String)
  object User {
    def parse(name: String, rawUUID: String): Either[String, User] = Try(UUID.fromString(rawUUID)) match {
      case Success(uuid) => Right(User(uuid, name))
      case Failure(t) => Left(t.getMessage)
    }
  }
  case class Destination(where: String) extends AnyVal

  sealed trait CorporationAlg[T]
  case class FindUser(name: String) extends CorporationAlg[Option[User]]
  case class VacationAllowance(id: UUID) extends CorporationAlg[Option[Int]]
  case class SendForVacation(id: UUID, howLong: Int) extends CorporationAlg[Destination]

  type Corporation[T] = Free[CorporationAlg, T]

  def findUser(name: String): Corporation[Option[User]] = Free.liftF(FindUser(name))
  def vacationAllowance(id: UUID): Corporation[Option[Int]] = Free.liftF(VacationAllowance(id))
  def sendForVacation(id: UUID, howLong: Int): Corporation[Destination] = Free.liftF(SendForVacation(id, howLong))

  //

  case class ErrorCode(code: Int) extends AnyVal

  sealed trait HttpAlg[T]
  case class Get(path: String) extends HttpAlg[Either[ErrorCode, String]]
  case class Post(path: String, body: String) extends HttpAlg[Either[ErrorCode, Unit]]

  type Http[T] = Free[HttpAlg, T]

  def get(path: String): Http[Either[ErrorCode, String]] = Free.liftF(Get(path))
  def post(path: String, body: String): Http[Either[ErrorCode, Unit]] = Free.liftF(Post(path, body))

  //

  sealed trait LoggingAlg[T]
  case class LogInfo(msg: String) extends LoggingAlg[Unit]

  type Logging[T] = Free[LoggingAlg, T]

  def logInfo(msg: String): Logging[Unit] = Free.liftF(LogInfo(msg))

  //

  // Corporation[Option[Destination]]
  val vacationProgram: OptionT[Corporation, Destination] = for {
    u <- OptionT(findUser("adam"))
    v <- OptionT(vacationAllowance(u.id))
    d <- OptionT.liftF(sendForVacation(u.id, v))
  } yield d

  val corporationToHttpInterpreter = new (CorporationAlg ~> EitherT[Http, String, ?]) {
    override def apply[A](fa: CorporationAlg[A]): EitherT[Http, String, A] = fa match {
      case FindUser(name) => EitherT[Http, String, Option[User]](get(s"/api/find_user?name=$name").map {
        case Right(response) => User.parse(name, response).map(Some(_))
        case Left(ErrorCode(404)) => Right(None)
        case Left(ErrorCode(c)) => Left(s"HTTP error code: $c")
      })

      case VacationAllowance(id) =>
        val allowanceFromApi: EitherT[Http, ErrorCode, String] = for {
          _ <- EitherT(post(s"/api/update_allowance?id=$id", s"timestamp=${System.currentTimeMillis()}"))
          n <- EitherT(get(s"/api/get_allowance?id=$id"))
        } yield n

        allowanceFromApi
          .subflatMap(n => Try(n.toInt).fold(t => Left(t.getMessage), Right(_)))
          .map(Option(_))
          .recover { case ErrorCode(404) => None }
          .leftMap { case ErrorCode(c) => s"HTTP error code: $c" }

      case SendForVacation(id, howLong) =>
        EitherT(post(s"/api?send_for_vacation?id=$id", s"how_long=$howLong"))
          .map(_ => Destination("Hawaii"))
          .leftMap { case ErrorCode(c) => s"HTTP error code: $c" }
    }
  }

  val httpToIdInterpreter = new (HttpAlg ~> Id) {
    override def apply[A](fa: HttpAlg[A]): Id[A] = fa match {
      case Get(path) => Left(ErrorCode(404))
      case Post(path, body) => Left(ErrorCode(404))
    }
  }

  def main(args: Array[String]): Unit = {
    println(vacationProgram
      .value.foldMap[EitherT[Http, String, ?]](corporationToHttpInterpreter)(catsDataMonadErrorForEitherT(implicitly[Monad[Http]]))
      .value.foldMap(httpToIdInterpreter))
  }
}
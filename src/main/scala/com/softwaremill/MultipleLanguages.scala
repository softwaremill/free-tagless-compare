package com.softwaremill

import java.util.UUID

import cats.data.Coproduct
import cats.free.{Free, Inject}
import cats.implicits._
import cats.{Monad, ~>}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MultipleLanguages {
  case class User(id: UUID, email: String, loyaltyPoints: Int)

  object Initial {
    trait UserRepository {
      def findUser(id: UUID): Future[Option[User]]
      def updateUser(u: User): Future[Unit]
    }

    trait EmailService {
      def sendEmail(email: String, subject: String, body: String): Future[Unit]
    }

    class LoyaltyPoints(ur: UserRepository, es: EmailService) {
      def addPoints(userId: UUID, pointsToAdd: Int): Future[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => Future.successful(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            for {
              _ <- ur.updateUser(updated)
              _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
            } yield Right(())
        }
      }
    }
  }

  object UsingFree {
    sealed trait UserRepositoryAlg[T]
    case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]
    case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

    class Users[F[_]](implicit i: Inject[UserRepositoryAlg, F]) {
      def findUser(id: UUID): Free[F, Option[User]] = Free.inject(FindUser(id))
      def updateUser(u: User): Free[F, Unit] = Free.inject(UpdateUser(u))
    }
    object Users {
      implicit def users[F[_]](implicit i: Inject[UserRepositoryAlg, F]): Users[F] = new Users
    }

    //

    sealed trait EmailAlg[T]
    case class SendEmail(email: String, subject: String, body: String) extends EmailAlg[Unit]

    class Emails[F[_]](implicit i: Inject[EmailAlg, F]) {
      def sendEmail(email: String, subject: String, body: String): Free[F, Unit] = Free.inject(SendEmail(email, subject, body))
    }
    object Emails {
      implicit def emails[F[_]](implicit i: Inject[EmailAlg, F]): Emails[F] = new Emails
    }

    //

    type UserAndEmailAlg[T] = Coproduct[UserRepositoryAlg, EmailAlg, T]

    def addPoints(userId: UUID, pointsToAdd: Int)(
      implicit ur: Users[UserAndEmailAlg], es: Emails[UserAndEmailAlg]): Free[UserAndEmailAlg, Either[String, Unit]] = {

      ur.findUser(userId).flatMap {
        case None => Free.pure(Left("User not found"))
        case Some(user) =>
          val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)

          for {
            _ <- ur.updateUser(updated)
            _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
          } yield Right(())
      }
    }

    val futureUserInterpreter = new (UserRepositoryAlg ~> Future) {
      override def apply[A](fa: UserRepositoryAlg[A]): Future[A] = fa match {
        case FindUser(id) => /* go and talk to a database */ Future.successful(None)
        case UpdateUser(u) => /* as above */ Future.successful(())
      }
    }

    val futureEmailInterpreter = new (EmailAlg ~> Future) {
      override def apply[A](fa: EmailAlg[A]): Future[A] = fa match {
        case SendEmail(email, subject, body) => /* use smtp */ Future.successful(())
      }
    }

    val futureUserOrEmailInterpreter = futureUserInterpreter or futureEmailInterpreter

    val result: Future[Either[String, Unit]] =
      addPoints(UUID.randomUUID(), 10).foldMap(futureUserOrEmailInterpreter)
  }

  object UsingTagless {
    trait UserRepositoryAlg[F[_]] {
      def findUser(id: UUID): F[Option[User]]
      def updateUser(u: User): F[Unit]
    }

    trait EmailAlg[F[_]] {
      def sendEmail(email: String, subject: String, body: String): F[Unit]
    }

    class LoyaltyPoints[F[_]: Monad](ur: UserRepositoryAlg[F], es: EmailAlg[F]) {
      def addPoints(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => implicitly[Monad[F]].pure(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            for {
              _ <- ur.updateUser(updated)
              _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
            } yield Right(())
        }
      }
    }

    trait FutureUserInterpreter extends UserRepositoryAlg[Future] {
      override def findUser(id: UUID): Future[Option[User]] =
        Future.successful(None) /* go and talk to a database */

      override def updateUser(u: User): Future[Unit] =
        Future.successful(()) /* as above */
    }

    trait FutureEmailInterpreter extends EmailAlg[Future] {
      override def sendEmail(email: String, subject: String, body: String): Future[Unit] =
        Future.successful(()) /* use smtp */
    }

    val result: Future[Either[String, Unit]] =
      new LoyaltyPoints(new FutureUserInterpreter {}, new FutureEmailInterpreter {}).addPoints(UUID.randomUUID(), 10)

  }
}

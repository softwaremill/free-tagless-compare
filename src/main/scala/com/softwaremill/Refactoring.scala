package com.softwaremill

import java.util.UUID

import cats.free.Free
import cats.{Monad, ~>}
import cats.implicits._
import com.softwaremill.Refactoring.Initial.User

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Refactoring {
  object Initial {
    trait EmailService {
      def sendEmail(email: String, subject: String, body: String): Future[Unit]
    }

    case class User(id: UUID, email: String, loyaltyPoints: Int)

    trait UserRepository {
      def findUser(id: UUID): Future[Option[User]]
      def updateUser(u: User): Future[Unit]
    }

    class LoyaltyPoints(ur: UserRepository) {
      def addPoints(userId: UUID, pointsToAdd: Int): Future[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => Future.successful(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            ur.updateUser(updated).map(_ => Right(()))
        }
      }
    }
  }

  object UsingFree {
    sealed trait UserRepositoryAlg[T]
    case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]
    case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

    type UserRepository[T] = Free[UserRepositoryAlg, T]

    def findUser(id: UUID): UserRepository[Option[User]] = Free.liftF(FindUser(id))
    def updateUser(u: User): UserRepository[Unit] = Free.liftF(UpdateUser(u))

    def addPoints(userId: UUID, pointsToAdd: Int): UserRepository[Either[String, Unit]] = {
      findUser(userId).flatMap {
        case None => Free.pure(Left("User not found"))
        case Some(user) =>
          val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
          updateUser(updated).map(_ => Right(()))
      }
    }

    val futureInterpreter = new (UserRepositoryAlg ~> Future) {
      override def apply[A](fa: UserRepositoryAlg[A]): Future[A] = fa match {
        case FindUser(id) => /* go and talk to a database */ Future.successful(None)
        case UpdateUser(u) => /* as above */ Future.successful(())
      }
    }

    val result: Future[Either[String, Unit]] =
      addPoints(UUID.randomUUID(), 10).foldMap(futureInterpreter)
  }

  object UsingTagless {
    trait UserRepositoryAlg[F[_]] {
      def findUser(id: UUID): F[Option[User]]
      def updateUser(u: User): F[Unit]
    }

    class LoyaltyPoints[F[_]: Monad](ur: UserRepositoryAlg[F]) {
      def addPoints(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
        ur.findUser(userId).flatMap {
          case None => implicitly[Monad[F]].pure(Left("User not found"))
          case Some(user) =>
            val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
            ur.updateUser(updated).map(_ => Right(()))
        }
      }
    }

    trait FutureInterpreter extends UserRepositoryAlg[Future] {
      override def findUser(id: UUID): Future[Option[User]] =
        Future.successful(None) /* go and talk to a database */

      override def updateUser(u: User): Future[Unit] =
        Future.successful(()) /* as above */
    }

    val result: Future[Either[String, Unit]] =
      new LoyaltyPoints(new FutureInterpreter {}).addPoints(UUID.randomUUID(), 10)

  }
}

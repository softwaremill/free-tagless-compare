package com.softwaremill

import java.util.UUID

import cats.free.Free
import cats.implicits._
import cats.{Monad, ~>}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Compile {
  case class User(id: UUID, email: String, loyaltyPoints: Int) {
    def serialize: String = id.toString + "," + loyaltyPoints + "," + email
  }
  object User {
    def parse(s: String): User = {
      val parts = s.split(",")
      User(UUID.fromString(parts(0)), parts(2), parts(1).toInt)
    }
  }


  object Initial {
    trait KVStore {
      def get(k: String): Future[Option[String]]
      def put(k: String, v: String): Future[Unit]
    }

    trait UserRepository {
      def findUser(id: UUID): Future[Option[User]]
      def updateUser(u: User): Future[Unit]
    }

    class UserRepositoryUsingKVStore(kvStore: KVStore) extends UserRepository {
      override def findUser(id: UUID): Future[Option[User]] =
        kvStore.get(id.toString).map(serialized => serialized.map(User.parse))

      override def updateUser(u: User): Future[Unit] = {
        val serialized = u.serialize
        for {
          _ <- kvStore.put(u.id.toString, serialized)
          _ <- kvStore.put(u.email, serialized) // we also maintain a by-email index
        } yield ()
      }
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

    //

    sealed trait KVAlg[T]
    case class Get(k: String) extends KVAlg[Option[String]]
    case class Put(k: String, v: String) extends KVAlg[Unit]

    type KV[T] = Free[KVAlg, T]

    def get(k: String): KV[Option[String]] = Free.liftF(Get(k))
    def put(k: String, v: String): KV[Unit] = Free.liftF(Put(k, v))

    //

    val userToKvInterpreter = new (UserRepositoryAlg ~> KV) {
      override def apply[A](fa: UserRepositoryAlg[A]): KV[A] = fa match {
        case FindUser(id) =>
          get(id.toString).map(_.map(User.parse))
        case UpdateUser(u) =>
          val serialized = u.serialize
          for {
            _ <- put(u.id.toString, serialized)
            _ <- put(u.email, serialized) // we also maintain a by-email index
          } yield()
      }
    }

    val kvToFutureInterpreter = new (KVAlg ~> Future) {
      override def apply[A](fa: KVAlg[A]): Future[A] = fa match {
        case Get(k) => /* go and talk to a database */ Future.successful(None)
        case Put(k, v) => /* as above */ Future.successful(())
      }
    }

    val result: Future[Either[String, Unit]] =
      addPoints(UUID.randomUUID(), 10)
        .foldMap(userToKvInterpreter)
        .foldMap(kvToFutureInterpreter)
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

    //

    trait KVAlg[F[_]] {
      def get(k: String): F[Option[String]]
      def put(k: String, v: String): F[Unit]
    }

    trait KvToFutureInterpreter extends KVAlg[Future] {
      override def get(k: String): Future[Option[String]] =
        Future.successful(None) /* go and talk to a database */

      override def put(k: String, v: String): Future[Unit] =
        Future.successful(()) /* as above */
    }

    //

    class UserThroughKvInterpreter[F[_]: Monad](kv: KVAlg[F]) extends UserRepositoryAlg[F] {
      override def findUser(id: UUID): F[Option[User]] =
        kv.get(id.toString).map(_.map(User.parse))

      override def updateUser(u: User): F[Unit] = {
        val serialized = u.serialize
        for {
          _ <- kv.put(u.id.toString, serialized)
          _ <- kv.put(u.email, serialized) // we also maintain a by-email index
        } yield ()
      }
    }

    val result: Future[Either[String, Unit]] =
      new LoyaltyPoints(new UserThroughKvInterpreter(new KvToFutureInterpreter {}))
        .addPoints(UUID.randomUUID(), 10)

  }
}

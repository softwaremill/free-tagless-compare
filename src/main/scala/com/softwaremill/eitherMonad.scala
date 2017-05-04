package com.softwaremill

import cats.{Functor, Monad}
import cats.data.EitherT
import cats.implicits._

object eitherMonad {
  trait EitherTFunctor[F[_], L] extends Functor[EitherT[F, L, ?]] {
    implicit val F: Functor[F]
    override def map[A, B](fa: EitherT[F, L, A])(f: A => B): EitherT[F, L, B] = fa map f
  }

  trait EitherTMonad[F[_], L] extends Monad[EitherT[F, L, ?]] with EitherTFunctor[F, L] {
    implicit val F: Monad[F]
    def pure[A](a: A): EitherT[F, L, A] = EitherT(F.pure(Either.right(a)))
    def flatMap[A, B](fa: EitherT[F, L, A])(f: A => EitherT[F, L, B]): EitherT[F, L, B] = fa flatMap f
    def tailRecM[A, B](a: A)(f: A => EitherT[F, L, Either[A, B]]): EitherT[F, L, B] =
      EitherT(F.tailRecM(a)(a0 => F.map(f(a0).value) {
        case Left(l)         => Right(Left(l))
        case Right(Left(a1)) => Left(a1)
        case Right(Right(b)) => Right(Right(b))
      }))
  }

  implicit def catsDataMonadErrorForEitherT[F[_], L](implicit F0: Monad[F]): Monad[EitherT[F, L, ?]] =
    new EitherTMonad[F, L] { implicit val F = F0 }
}

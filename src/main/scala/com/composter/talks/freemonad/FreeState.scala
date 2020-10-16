package com.composter.talks.freemonad
import cats.Functor

sealed trait StateF[S, A]
import cats.free.Free

object StateF {
  case class Get[S, A](f: S => A) extends StateF[S, A]
  case class Put[S, A](s: S, a: A) extends StateF[S, A]

  implicit def statefFun[S] =
    new Functor[({ type λ[α] = StateF[S, α] })#λ] {
      def map[A, B](m: StateF[S, A])(f: A => B) =
        m match {
          case Get(g) => Get((s: S) => f(g(s)))
          case Put(s, a) => Put(s, f(a))
        }
    }
}

object FreeState {
  import StateF._

  type FreeState[S, A] = cats.free.Free[({ type λ[α] = StateF[S, α] })#λ, A]

  def getState[S]: FreeState[S, S] = {
    Free.liftF(Get((s: S) => s))
  }

  def putState[S](s: S): FreeState[S, Unit] = {
    Free.liftF(Put(s, ()))
  }

  def pureState[S, A](a: A): FreeState[S, A] = {
    Free.pure(a)
  }

  def evalS[S, A](s: S, t: FreeState[S, A]): A =
    t.resume match {
      case Left(Get(f)) => evalS(s, f(s))
      case Left(Put(n, a)) => evalS(n, a)
      case Right(a) => a
    }
}

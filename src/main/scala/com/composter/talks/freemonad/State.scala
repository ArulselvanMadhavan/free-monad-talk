package com.composter.talks.freemonad

case class State[S, +A](runS: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State[S, B]((s: S) => {
      val (a, s1): (A, S) = runS(s)
      (f(a), s1)
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State[S, B]((s: S) => {
      val (a, s1) = runS(s) // pushes values of this function into stack
      f(a) runS s1
    })
  }
}

object State {
  def pure[S, A](a: A): State[S, A] = {
    State[S, A]((s: S) => (a, s))
  }

  def getState[S]: State[S, S] = State[S, S](s => (s, s))

  def putState[S](s: S): State[S, Unit] = State[S, Unit](_ => () -> s)

}

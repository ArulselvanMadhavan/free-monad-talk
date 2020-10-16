package examples

object StateExample {
  // StateMonad: s -> (a, s)
  case class State[S, +A](runS: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = {
      State[S, B]((s) => {
        val (a, s1) = runS(s)
        (f(a), s1)
      })
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State[S, B](s => {
        val (a, s1) = runS(s)
        f(a) runS s1
      })
    }
  }

  def getState[S]: State[S, S] = State[S, S](s => s -> s)
  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))
  def pureState[S, A](a: A): State[S, A] = State[S, A](s => (a, s))

  // Output: (0, a0), (1, a2), ....
  def zipIndex[A](as: List[A]): List[(Int, A)] = {
    FoldExample
      .foldLeft[A, State[Int, List[(Int, A)]]](as)(pureState[Int, List[(Int, A)]](List()))(
        (acc, a) =>
          for {
            xs <- acc
            n <- getState
            _ <- setState(n + 1)
          } yield (n, a) :: xs
      )
      .runS(0)
      ._1
      .reverse
  }

  // Why does this fail with stack overflow?
  // It builds a list of functions State(s2 => (s1 => (0, (0 => (s, s)).apply(s1)))(s2))
}

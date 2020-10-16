package examples

import MutualRecursionExample.Trampoline
import Trampoline._

object StateTrampoline {

  case class State[S, +A](runS: S => Trampoline[(A, S)]) {

    def map[B](f: A => B): State[S, B] = {
      State[S, B]((s) => {
        val (a, s1) = runS(s).runT // Running a computation. If you aren't running at the tail
        Done((f(a), s1)) // Why Done?
      })
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State[S, B](s => {
        val (a, s1) = runS(s).runT // Not in a tail call. Can't be wrapped in Trampoline
        More(() => f(a) runS s1) // Why More? and Not Done?
      })
    }

  }

  def pureState[S, A](a: A): State[S, A] = State[S, A](s => Done((a, s)))
  def getState[S]: State[S, S] = State[S, S](s => Done(s -> s))
  def setState[S](s: S): State[S, Unit] = State(_ => Done(((), s)))

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
      .runT // Notice the RunT
      ._1
      .reverse
  }

}

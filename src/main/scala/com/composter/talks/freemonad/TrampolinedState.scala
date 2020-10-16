package com.composter.talks.freemonad
import Trampoline._

// **Version 2
case class TrampolinedState[S, A](runS: S => Trampoline[(A, S)]) {

  def map[B](f: A => B): TrampolinedState[S, B] = {
    flatMap[B]((a) => TrampolinedState[S, B](s => Done(f(a) -> s)))
  }

  def flatMap[B](f: A => TrampolinedState[S, B]): TrampolinedState[S, B] = {
    TrampolinedState[S, B](
      (s: S) =>
        More(() => {
          RunAndProcess(runS(s), (res: (A, S)) => { val (a, s1) = res; f(a).runS(s1) })
        })
    )
  }

}

object TrampolinedState {
  def pure[S, A](a: A): TrampolinedState[S, A] = {
    TrampolinedState[S, A]((s: S) => Done(a -> s))
  }

  def getState[S]: TrampolinedState[S, S] = TrampolinedState[S, S](s => Done(s -> s))

  def putState[S](s: S): TrampolinedState[S, Unit] = TrampolinedState[S, Unit](_ => Done(() -> s))
}

// case class TrampolinedState[S, A](runS: S => Trampoline[(A, S)]) {
//   def map[B](f: A => B): TrampolinedState[S, B] = {
//     flatMap[B]((a) => TrampolinedState(s => Done((f(a), s))))
//   }

// **Version 1
//   def flatMap[B](f: A => TrampolinedState[S, B]): TrampolinedState[S, B] = {
//     TrampolinedState[S, B](
//       (s: S) =>
//         More(() => {
//           runS(s).flatMap {
//             case (a, s2) => f(a).runS(s2)
//           }
//         })
//     )
//   }
// }

// object TrampolinedState {
//   def pure[S, A](a: A): TrampolinedState[S, A] = {
//     TrampolinedState[S, A](s => Done(a -> s))
//   }

//   def getState[S]: TrampolinedState[S, S] = TrampolinedState[S, S](s => Done(s -> s))

//   def putState[S](s: S): TrampolinedState[S, Unit] = TrampolinedState[S, Unit](_ => Done(() -> s))
// }

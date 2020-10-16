package com.composter.talks.freemonad
import scala.annotation.tailrec

sealed trait Trampoline[A] {
  import Trampoline._

  @tailrec
  final def step: Either[() => Trampoline[A], A] = this match {
    case Done(a) => Right(a)
    case More(f) => Left(f)
    case RunAndProcess(ta, f) =>
      ta match {
        case Done(a) => f(a).step
        case More(k) => Left(() => RunAndProcess(k(), f))
        case RunAndProcess(tb, g) =>
          (RunAndProcess(tb, (x: Any) => RunAndProcess(g(x), f)): Trampoline[A]).step
      }
  }

  @tailrec
  final def runT: A = {
    step match {
      case Left(f) => f().runT
      case Right(a) => a
    }
  }
}

object Trampoline {
  case class Done[A](a: A) extends Trampoline[A]
  case class More[A](f: () => Trampoline[A]) extends Trampoline[A]
  case class RunAndProcess[A, B](value: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

}

// sealed trait Trampoline[A] {
//   import Trampoline._

//   @tailrec
//   final def runT: A = this match {
//     case Done(a) => a
//     case More(f) => f().runT
//     case RunAndProcess(tb, f) =>
//       tb match {
//         case Done(b) => f(b).runT
//         case More(k) => (RunAndProcess(k(), f): Trampoline[A]).runT
//         case RunAndProcess(b, g) =>
//           (RunAndProcess(b, (r: Any) => RunAndProcess(g(r), f)): Trampoline[A]).runT
//       }
//   }
// }

// object Trampoline {
//   case class Done[A](result: A) extends Trampoline[A]
//   case class More[A](func: () => Trampoline[A]) extends Trampoline[A]
//   case class RunAndProcess[A, B](value: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]
// }

// sealed trait Trampoline[+A] {
//   import Trampoline._

//   final def resume: Either[() => Trampoline[A], A] = this match {
//     case Done(a) => Right(a)
//     case More(k) => Left(k)
//     case FlatMap(ta, f) =>
//       ta match {
//         case Done(a) => f(a).resume
//         case More(sus) => Left(() => FlatMap(sus(), f))
//         case FlatMap(tb, g) => tb.flatMap((b) => g(b).flatMap(f)).resume
//       }
//   }
//   // Main control loop
//   final def runT: A = resume match {
//     case Right(a) => a
//     case Left(f) => f().runT
//   }

//   def map[B](f: A => B): Trampoline[B] = {
//     flatMap[B]((a) => Done(f(a)))
//   }

//   def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = {
//     this match {
//       case FlatMap(ta, g) => FlatMap(ta, (a: Any) => g(a).flatMap(f))
//       case x => FlatMap(x, f)
//     }
//   }
// }

// object Trampoline {
//   // GADT
//   case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
//   case class Done[+A](result: A) extends Trampoline[A]

//   private case class FlatMap[A, +B](sub: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]
// }

package examples
import scala.annotation.tailrec

object StateFlatMap {
  sealed trait TrampolineF[+A] {
    import TrampolineF._
    @tailrec
    final def runT: A =
      this match {
        case MoreF(k) => k().runT
        case DoneF(v) => v
      }

    // Still runT is called from a different place. Maybe represent FlatMap as Data and execute in a control loop?
    def flatMap[B](f: A => TrampolineF[B]) = MoreF(() => f(runT))
  }

  object TrampolineF {
    case class MoreF[+A](k: () => TrampolineF[A]) extends TrampolineF[A]
    case class DoneF[+A](result: A) extends TrampolineF[A]
  }

  // Represent flatmap as Data
  sealed trait Trampoline[+A] {
    import Trampoline._
    // @tailrec
    // final def runT: A =
    //   resume match {
    //     case Right(a) => a
    //     case Left(k) => k().runT
    //   }

    def map[B](f: A => B): Trampoline[B] = {
      flatMap[B]((a) => Done(f(a)))
    }

    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
      this match {
        case FlatMap(a, g) =>
          FlatMap(a, (x: Any) => g(x) flatMap f) // Left associated flatMap to right associated flatMap.
        case x => FlatMap(x, f)
      }

    // def resume: Either[() => Trampoline[A], A] = this match {
    //   case Done(v) => Right(v)
    //   case More(k) => Left(k)
    //   case FlatMap(a, f) =>
    //     a match {
    //       case Done(v) => f(v).resume
    //       case More(k) => Left(() => k() flatMap f)
    //       case FlatMap(b, g) => b.flatMap((x: Any) => g(x) flatMap f).resume
    //     }
    // }
  }

  object Trampoline {
    case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
    case class Done[+A](result: A) extends Trampoline[A]
    private case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B])
        extends Trampoline[B]
  }
}

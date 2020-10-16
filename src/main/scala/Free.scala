package examples
import scala.annotation.tailrec

sealed abstract class Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

sealed trait Free[S[+_], +A] {
  import Free._
  def map[B](f: A => B): Free[S, B] =
    flatMap(a => Done(f(a)))

  def flatMap[B](f: A => Free[S, B]): Free[S, B] = {
    this match {
      case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x).flatMap(f))
      case x => FlatMap(x, f)
    }
  }

  @tailrec
  final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] =
    this match {
      case Done(a) => Right(a)
      case More(k) => Left(k)
      case a FlatMap f =>
        a match {
          case Done(a) => f(a).resume
          case More(k) => Left(S.map(k)(_ flatMap f))
          case b FlatMap g => b.flatMap((x: Any) => g(x) flatMap f).resume
        }
    }
}

object Free {
  case class Done[S[+_], A](a: A) extends Free[S, A]
  case class More[S[+_], A](s: S[Free[S, A]]) extends Free[S, A]
  private case class FlatMap[S[+_], A, B](s: Free[S, A], f: A => Free[S, B]) extends Free[S, B]

  type Pair[+A] = (A, A)
  type BinTree[+A] = Free[Pair, A]

  type TreeList[A] = Free[List, A]

  type Trampoline[+A] = Free[Function0, A] // Stack safe recursion
  object Trampoline {

    implicit val f0 = new Functor[Function0] {
      def map[A, B](fa: Function0[A])(f: A => B): Function0[B] = { () =>
        f(fa())
      }
    }

    @tailrec
    def runT[A](t: Trampoline[A]): A = t.resume match {
      case Left(sfa) => runT(sfa())
      case Right(a) => a
    }
  }

  // What can we do for State?
  sealed trait StateF[S, +A]
  case class Get[S, A](f: S => A) extends StateF[S, A]
  case class Put[S, A](s: S, a: A) extends StateF[S, A]

  object StateF {
    // type StateFunc[S, ]
    implicit def stateFunctor[S] = new Functor[StateF[S, *]] {
      def map[A, B](fa: StateF[S, A])(f: A => B): StateF[S, B] = {
        fa match {
          case Get(g) => Get((s: S) => f(g(s)))
          case Put(s, a) => Put(s, f(a))
        }
      }
    }
  }

  type FreeState[S, +A] = Free[StateF[S, +*], A]

  object FreeState {
    def pureState[S, A](a: A): FreeState[S, A] =
      Done(a)

    def getState[S]: FreeState[S, S] =
      More(Get(s => Done(s)))

    def setState[S](s: S): FreeState[S, Unit] =
      More(Put(s, Done(())))

    @tailrec
    def evalS[S, A](s: S, t: FreeState[S, A]): A =
      t.resume match {
        case Left(Get(f)) => evalS(s, f(s))
        case Left(Put(n, a)) => evalS(n, a)
        case Right(a) => a
      }

    def zipIndex[A](as: List[A]): List[(Int, A)] =
      evalS(0, as.foldLeft(pureState[Int, List[(Int, A)]](List())) { (acc, a) =>
        for {
          xs <- acc
          n <- getState
          _ <- setState(n + 1)
        } yield (n, a) :: xs
      }).reverse
  }
}

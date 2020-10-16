package examples
import scala.annotation.tailrec

object MutualRecursionExample {
  // @tailrec
  def even[A](ns: List[A]): Boolean =
    ns match {
      case Nil => true
      case x :: xs => odd(xs)
    }

  def odd[A](ns: List[A]): Boolean =
    ns match {
      case Nil => false
      case x :: xs => even(xs)
    }

  // Solution: Stack <<< Heap
  // Move recursive calls that take up the stack into the heap - Trampoline
  // https://en.wikipedia.org/wiki/Trampoline_(computing)
  // Trampoline - suspend calls that take up the stack; move it to heap; execute these functions using a control loop by maintaining constant space

  sealed trait Trampoline[+A] {
    import Trampoline._
    @tailrec
    final def runT: A = this match {
      case More(f) => f().runT
      case Done(a) => a
    }
  }

  object Trampoline {
    case class More[+A](f: () => Trampoline[A]) extends Trampoline[A]
    case class Done[+A](a: A) extends Trampoline[A]

    def even[A](ns: List[A]): Trampoline[Boolean] =
      ns match {
        case Nil => Done(true)
        case x :: xs => More(() => odd(xs))
      }

    def odd[A](ns: List[A]): Trampoline[Boolean] =
      ns match {
        case Nil => Done(false)
        case x :: xs => More(() => even(xs))
      }
  }
}

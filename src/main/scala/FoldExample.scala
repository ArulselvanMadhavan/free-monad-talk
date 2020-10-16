package examples
import scala.annotation.tailrec

object FoldExample {

  // Write your program recursively.
  // scala compiler detects self recursive calls and replaces the tail call and its call stack with a single JMP instruction that is semantically equivalent to iterative loop
  def foldLeft[A, B](as: List[A])(init: B)(f: (B, A) => B): B = {
    @tailrec
    def go(xs: List[A], acc: B): B = {
      xs match {
        case Nil => acc
        case y :: ys => go(ys, f(acc, y))
      }
    }
    go(as, init)
  }

  // Problems
  // 1. Mutual recursion doesn't work
}

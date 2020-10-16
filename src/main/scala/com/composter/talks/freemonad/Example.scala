package com.composter.talks.freemonad
import scala.annotation.tailrec
import Trampoline._

object Example {
  def addNumbersToList[A](xs: List[A]): List[(Int, A)] = {
    xs.foldLeft(State.pure[Int, List[(Int, A)]](List.empty[(Int, A)]))(
        (acc: State[Int, List[(Int, A)]], a: A) => {
          for {
            (xs) <- acc
            s <- State.getState[Int]
            _ <- State.putState[Int](s + 1)
          } yield (s, a) :: xs
        }
      )
      .runS(0)
      ._1
      .reverse
  }

  def addNumbersWithTrampolinedState[A](xs: List[A]): List[(Int, A)] = {
    xs.foldLeft(TrampolinedState.pure[Int, List[(Int, A)]](List.empty[(Int, A)]))(
        (acc: TrampolinedState[Int, List[(Int, A)]], a: A) => {
          for {
            (xs) <- acc
            s <- TrampolinedState.getState[Int]
            _ <- TrampolinedState.putState[Int](s + 1)
          } yield (s, a) :: xs
        }
      )
      .runS(0)
      .runT
      ._1
      .reverse
  }

  // def addNumbersWithStateF[A](xs: List[A]): List[(Int, A)] = {
  //   FreeState.evalS(
  //     0,
  //     xs.foldLeft(FreeState.pureState[Int, List[(Int, A)]](List.empty[(Int, A)]))(
  //       (acc: FreeState.FreeState[Int, List[(Int, A)]], a: A) => {
  //         for {
  //           (xs) <- acc
  //           s <- FreeState.getState[Int]
  //           _ <- FreeState.putState[Int](s + 1)
  //         } yield (s, a) :: xs
  //       }
  //     )
  //   )
  // }

  // @tailrec
  def unsafeFac(n: Int): Int = {
    if (n == 0) 1
    else n * unsafeFac(n - 1)
  }

  def unsafeEven(n: Int): Boolean = {
    if (n == 0) true
    else unsafeOdd(n - 1)
  }

  def unsafeOdd(n: Int): Boolean = {
    if (n == 0) false
    else unsafeEven(n - 1)
  }

  def safeEven(n: Int): Trampoline[Boolean] = {
    if (n == 0) Done(true)
    else More(() => safeOdd(n - 1))
  }

  def safeOdd(n: Int): Trampoline[Boolean] = {
    if (n == 0) Done(false)
    else More(() => safeEven(n - 1))
  }

  def safeFac(n: Long): Trampoline[Long] = {
    if (n == 0) Done(1L)
    else RunAndProcess[Long, Long](More(() => safeFac(n - 1)), (r: Long) => Done(n * r))
  }
}

// package examples

// import cats.effect._
// // https://github.com/typelevel/cats/blob/4eab3d6facc7b8cc0420fadfe48609329616f5e6/core/src/main/scala/cats/Eval.scala#L35
// object CatsEff {
//   def zipIndex[A](as: List[A]): List[(Int, A)] = {
//     evalS(0, as.foldLeft(pureState[Int, List[(Int, A)]](List())) { (acc, a) =>
//       for {
//         xs <- acc
//         n <- getState
//         _ <- setState(n + 1)
//       } yield (n, a) :: xs
//     }).reverse
//   }
// }

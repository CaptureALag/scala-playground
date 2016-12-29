package cf691.d

import Utils.IO._

object Solution extends App {
  val Seq(n, m) = readInts()
  val permutation = readInts()

  val edges =
    for(_ <- 1 to m) yield
      readInts().map(_ - 1) match {
        case Seq(a, b) => (a, b)
      }

  val graph = new Graph(n, edges)

  val maximizedPermutation = SwapLexicographicMaximizer(graph, permutation)(-_)

  println(maximizedPermutation.mkString(" "))
}

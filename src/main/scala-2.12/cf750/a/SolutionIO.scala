package cf750.a

import Utils.IO._

object SolutionIO extends App {
  val Seq(n, k) = readInts()
  println(new Solution(n, k).answer)
}

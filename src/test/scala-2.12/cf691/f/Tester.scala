package cf691.f

object Tester extends App {
  val testcases : Seq[(Seq[Int], Seq[Int])] = Seq(
    (Seq(4, 2, 6, 1, 3), Seq(1, 3, 5, 8))
  )

  for((a, p) <- testcases) {
    println(new Solution(a, p).answers)
  }
}

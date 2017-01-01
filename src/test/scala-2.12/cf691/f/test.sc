import cf691.f._

val testcases : Seq[(Seq[Int], Seq[Int])] = Seq(
  (Seq(4, 2, 6, 1, 3), Seq(1, 3, 5, 8)),
  (Seq(5, 6), Seq(30, 31))
)

for((a, p) <- testcases) {
  println(new Solution(a, p).answers)
}
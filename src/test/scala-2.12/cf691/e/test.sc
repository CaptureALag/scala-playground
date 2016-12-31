import cf691.e._

val testcases : Seq[(Int, Long, Seq[Long])] = Seq(
  (5, Seq(15, 1, 2, 4, 8))
).flatMap({
  case (n, a) =>
    (1L to 10L).map((n, _, a.map(_.toLong)))
})

for((n, k, a) <- testcases) {
  println(new Solution(n, k, a).answer)
}
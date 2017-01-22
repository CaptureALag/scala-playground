import cf750.d.Solution

val testcases : Seq[(Seq[Int], Int)] = Seq(
  Seq(4,2,2,3) -> 39,
  Seq(1,1,1,1,1,3) -> 85,
  Seq(3) -> 3
)

for ((test, ans) <- testcases) {
  assert(Solution(test : _*) == ans)
}

println("success")
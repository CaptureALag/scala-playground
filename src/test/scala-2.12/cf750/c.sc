import cf750.c.Solution

val testcases : Seq[(Seq[(Int, Int)], String)] = Seq(
  Seq(
    (-7, 1),
    (5, 2),
    (8, 2)
  ) -> "1907",
  Seq(
    (57, 1),
    (22, 2)
  ) -> "Impossible",
  Seq(
    (-5, 1)
  ) -> "Infinity",
  Seq(
    (27, 2),
    (13, 1),
    (-50, 1),
    (8, 2)
  ) -> "1897"
)

for((test, res) <- testcases) {
  assert(Solution(test) == res, test)
}
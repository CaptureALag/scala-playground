val testcases : Seq[((Int, Int), Int)] = Seq(
  (3, 222) -> 2,
  (4, 190) -> 4,
  (7, 1) -> 7
)

import cf750.a.NumberOfTasks._
import cf750.a.Solution
import cf750.a.TimeToParty._

for (test@((n, k), res) <- testcases) {
  assert(new Solution(n, k).answer == res, test)
}
import cf750.b.{Direction, Solution}
import cf750.b.Direction._

val testcases: Seq[(Seq[(Int, Direction.Value)], Boolean)] = Seq(
  Seq(
    (7500, South),
    (10000, East),
    (3500, North),
    (4444, West),
    (4000, North)
  ) -> true,
  Seq(
    (15000, South),
    (4000, East)
  ) -> false,
  Seq(
    (20000, South),
    (1000, North),
    (1000000, West),
    (9000, North),
    (10000, North)
  ) -> true,
  Seq(
    (20000, South),
    (1000, East),
    (20000, North)
  ) -> false,
  Seq(
    (1000, North),
    (1000, South)
  ) -> false,
  Seq(
    (50, South),
    (50, North),
    (15000, South),
    (15000, North)
  ) -> true
)

for ((test, res) <- testcases) {
  val solution = new Solution(test)
  assert(solution.answer == res, s"$test: expected $res, got ${solution.res}")
}

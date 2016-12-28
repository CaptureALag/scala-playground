import cf691.b.SPalyndromeChecker

val testcases : Seq[(String, Boolean)] =
  Seq(
    "oXoxoXo" -> true,
    "bod" -> true,
    "bOd" -> true,
    "bid" -> false,
    "ER" -> false,
    "kekek" -> false,
    "o" -> true,
    "oOo" -> true,
    "oo" -> true,
    "oO" -> false
  )

for ((test, res) <- testcases) {
  assert(SPalyndromeChecker(test) == res, test)
}

println("All tests passed")
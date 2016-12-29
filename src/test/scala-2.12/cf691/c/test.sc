import cf691.c.SimpleExpFormatter

import scalaz.\/-

val testcases : Seq[(String, String)] = Seq(
  "16" -> "1.6E1",
  "01.2340" -> "1.234",
  ".100" -> "1E-1",
  "100.0" -> "1E2",
  "00000.0000" -> "0"
)

for((input, res) <- testcases) {
  assert(SimpleExpFormatter(input) == \/-(res), input)
}
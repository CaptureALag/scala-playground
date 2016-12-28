package cf691.b

/**
  * use
  * LetterSymmetryChecker(a, b) to check if a and b are symmetric
  * LetterSymmeryChecker(a) to check if a is symmetric to itself
  */
object LetterSymmetryChecker extends Function2[Char, Char, Boolean] {
  val symLetters : Seq[Char] =
    Seq('H', 'I', 'M', 'O', 'o', 'T', 'U', 'V', 'v', 'W', 'w', 'X', 'x', 'Y')

  val symLetterPairs : Set[(Char, Char)] =
    ( symLetters.map(x => x -> x) ++
      Seq('p' -> 'q', 'b' -> 'd').flatMap({ case (c1, c2) => Seq(c1 -> c2, c2 -> c1)})
    ).toSet

  override def apply(c1: Char, c2: Char): Boolean = symLetterPairs.contains(c1, c2)

  def apply(c : Char) : Boolean = apply(c, c)
}

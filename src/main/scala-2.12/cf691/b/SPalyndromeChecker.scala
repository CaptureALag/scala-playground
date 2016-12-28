package cf691.b

import scalaz._, Scalaz._

object SPalyndromeChecker extends ((String) => Boolean) {
 override def apply(s: String): Boolean =
   StringMiddleSplitter(s) match {
      case (left : String, midLetter : Option[Char], right : String) =>
         Tag.unwrap(Stream[Boolean](
            midLetter.forall(LetterSymmetryChecker(_)),
            {
              val zippedReflections = left.zip(right.reverseIterator.toSeq)
              zippedReflections.forall(Function.tupled(LetterSymmetryChecker(_, _)))
            }
         ).map(Tags.Conjunction[Boolean]).suml)
   }
}

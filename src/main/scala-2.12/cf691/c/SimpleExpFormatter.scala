package cf691.c

import scalaz._

object SimpleExpFormatter extends ((String) => String \/ String) {
  override def apply(s: String) : String \/ String =
    Number(s) match {
      case Some(number) =>
        val exp = number.getMaxPowOf10.getOrElse(0)
        val normalizedNumber = number.multiplyByPowOf10(-exp)
        val normalizedNumberString = normalizedNumber.toString
        \/-(normalizedNumberString + (if(exp != 0) "E" + exp else ""))
      case None =>
        -\/("Failed to parse number from given string")
    }
}

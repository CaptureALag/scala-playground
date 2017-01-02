package Utils

object Bool2String {
  def apply(b : Boolean)(implicit trueLabel : String = "YES", falseLabel : String = "NO"): String =
    if (b) {
      trueLabel
    } else {
      falseLabel
    }
}

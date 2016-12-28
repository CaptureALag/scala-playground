package cf691.b

object StringMiddleSplitter extends Function1[String, (String, Option[Char], String)] {
  override def apply(inputString: String): (String, Option[Char], String) = {
    val middle = inputString.length / 2
    inputString.toVector.splitAt(middle) match {
      case (xs, ys) if xs.length == ys.length => (xs.mkString, None, ys.mkString)
      case (xs, mid +: ys) if xs.length == ys.length => (xs.mkString, Some(mid), ys.mkString)
    }
  }
}

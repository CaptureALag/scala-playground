package cf691.c

class Number(rawIntPart : Seq[Char], rawRealPart : Seq[Char]) {
  val intPart : Vector[Char] = rawIntPart.dropWhile(_ == '0').toVector
  val realPart : Vector[Char] = rawRealPart.reverse.dropWhile(_ == '0').reverse.toVector

  def getMaxPowOf10 : Option[Int] =
    (
      if(intPart.isEmpty) {
        realPart.zip(Stream.from(-1, -1))
      } else {
        intPart.reverse.zip(Stream.from(0)).reverse
      }
    ).find(_._1 != '0').map(_._2)

  def multiplyByPowOf10(pow : Int) : Number =
    pow match {
      case 0 => this
      case x if x > 0 =>
        val (realPartToMove, newRealPart) = realPart.splitAt(x)
        new Number(intPart ++ realPartToMove, newRealPart)
      case x if x < 0 =>
        val (newIntPart, intPartToMove) = intPart.splitAt(intPart.size + x)
        new Number(newIntPart, intPartToMove ++ realPart)
    }

  override def toString: String =
    if(intPart.isEmpty && realPart.isEmpty)
      "0"
    else
      s"${intPart.mkString}${if(realPart.isEmpty) "" else "."}${realPart.mkString}"
}

object Number {
 def apply(num : String) : Option[Number] = {
   num split "\\." match {
     case Array(intPart) =>
       Some(new Number(intPart, Seq()))
     case Array(intPart, realPart) =>
       Some(new Number(intPart, realPart))
     case _ =>
       None
   }
 }
}

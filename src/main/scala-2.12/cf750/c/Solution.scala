package cf750.c

object Solution extends (Seq[(Int, Int)] => String) {
  override def apply(input: Seq[(Int, Int)]) : String = {

    val (div2, div1, res) = PartitionedDeltas(input)

    implicit class OptionOps(seq : Seq[Int]) {
      def maxOption: Option[Int] = seq match {
        case Seq() => None
        case xs => Some(xs.max)
      }
      def minOption: Option[Int] = seq match {
        case Seq() => None
        case xs => Some(xs.min)
      }
    }

    (div2.maxOption, div1.minOption) match {
      case (None, _) => "Infinity"
      case (Some(div2max), Some(div1min)) if div2max >= div1min => "Impossible"
      case (Some(div2max), _) => (1899 + res - div2max).toString
    }
  }
}

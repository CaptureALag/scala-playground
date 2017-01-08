package cf750.c

object PartitionedDeltas extends (Seq[(Int, Int)] => (Seq[Int], Seq[Int], Int)) {
  override def apply(input: Seq[(Int, Int)]) : (Seq[Int], Seq[Int], Int) = {
    val zipped = input.map(_._1).scanLeft(0)(_ + _).zip(input.map(_._2) :+ 0)
    val grouped = zipped.groupBy(_._2).mapValues(_.map(_._1))
    (grouped.getOrElse(2, Seq()), grouped.getOrElse(1, Seq()), grouped(0).head)
  }
}

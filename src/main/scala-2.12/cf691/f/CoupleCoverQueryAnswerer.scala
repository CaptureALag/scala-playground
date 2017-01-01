package cf691.f

import scala.collection.immutable.{HashMap, Iterable, SortedMap}

class CoupleCoverQueryAnswerer(val balls : Seq[Int], productLimit : Int) {

  val n : Int = balls.length
//  println(n)
  val ballCounts : SortedMap[Int, Int] = SortedMap(balls.groupBy(identity).mapValues(_.size).toSeq : _*)

//  println("Ball counts")
//  println(ballCounts)

  val productCounts : SortedMap[Int, Int] =
    SortedMap(
      ballCounts.keysIterator.toStream.flatMap(
        x =>
          ballCounts.keysIterator
          .takeWhile(_ * x <= productLimit)
          .map(y => (x * y, ballCounts(y) * (ballCounts(x) - (if(x == y) 1 else 0))))
      ).groupBy(x => x._1)
       .mapValues(_.map(_._2).sum)
       .toArray : _*
    )

//  println("Product counts")
//  println(productCounts)

  val partialSums: SortedMap[Int, Int] =
    productCounts.scanLeft(0 -> 0)({
      case ((_, prSum), (i, sum)) => (i, prSum + sum)
    })

//  println("Partial sums")
//  println(partialSums)

  def apply(p : Int): Int = {
    n * (n - 1) - partialSums.rangeImpl(None, Some(p)).lastOption.map(_._2).getOrElse(0)
  }
}

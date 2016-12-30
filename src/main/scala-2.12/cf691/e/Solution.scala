package cf691.e

import Utils.IO._
import scalaz._, Scalaz._

object Solution extends App {
  val Seq(n, k) = readLongs()
  val indexedA = readLongs().zipWithIndex
  val goodPairs = indexedA.flatMap(x => indexedA.toStream.strengthL(x)).filter({
    case ((a, _), (b, _)) => XorBitCounter(a, b) % 3 == 0
  })
  val goodPairsIndice = goodPairs.flatMap({
    case ((_, i), (_, j)) => Seq((i, j), (j, i))
  }).toSet
  val initialMatrix = new Matrix[Long](
    (0 until n.toInt).map(i => (0 until n.toInt).map(j => if(goodPairsIndice.contains((i, j))) 1L else 0L))
  )
  val finalMatrix = initialMatrix.power(k - 1)
  println(finalMatrix.sum)
}

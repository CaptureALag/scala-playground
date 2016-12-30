package cf691.e

import scala.collection.immutable.BitSet

object XorBitCounter extends ((Long, Long) => Int) {
  override def apply(v1: Long, v2: Long) : Int = {
    val res = BitSet.fromBitMaskNoCopy(Array(v1 ^ v2)).size
    res
  }
}

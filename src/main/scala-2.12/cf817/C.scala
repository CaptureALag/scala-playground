package cf817

import scala.io.StdIn._

object C extends App {
  def readLongs() = readLine() split " " map (_.toLong) toSeq;

  val Seq(n, s) = readLongs()

  def f(x : Long): Long = x - x.toString.map(x => (x + "").toInt).sum

  def findLowerBound(lo : Long, hi : Long) : Long = {
    if(lo == hi) {
      lo
    } else if(lo + 1 == hi) {
      if(f(lo) < s) hi else lo
    } else {
      val mid = (lo + hi) / 2
      if(f(mid) < s) {
        findLowerBound(mid, hi)
      } else {
        findLowerBound(lo, mid)
      }
    }
  }

  println(
    if(f(n) < s) {
      0
    } else {
      n - findLowerBound(1, n + 1) + 1
    }
  )


}
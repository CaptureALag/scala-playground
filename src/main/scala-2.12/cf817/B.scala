package cf817

import scala.io.StdIn._

object B extends App {
  readLine()
  def readInts() = readLine() split " " map (_.toInt) toSeq;

  val a = readInts()
  val min3 = a.sorted.take(3)

  def newtonBinom(n: Long, k: Long): Long = (n - k + 1 to n).product / (1.toLong to k).product

  println(
    min3 groupBy(x => x) mapValues (_.length.toLong) map { case (x, n) =>
      newtonBinom(a.count(_ == x) , Math.min(n, 3))
    } product
  )
}
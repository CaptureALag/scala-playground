package Utils

import scala.io.StdIn

object IO {
  def readTokens() : Seq[String] = StdIn.readLine().split("\\s+")
  def readInts() : Seq[Int] = readTokens().map(_.toInt)
  def readLongs() : Seq[Long] = readTokens().map(_.toLong)

  def readPairOfInts() : (Int, Int) = readInts() match {
    case Seq(a, b) => (a, b)
  }
}

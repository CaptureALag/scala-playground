package Utils

import scala.io.StdIn

object IO {
  def readString() : String = StdIn.readLine()
  def readTokens() : Seq[String] = readString().split("\\s+")
  def readInt() : Int = readString().toInt
  def readInts() : Seq[Int] = readTokens().map(_.toInt)
  def readLongs() : Seq[Long] = readTokens().map(_.toLong)

  def readPairOfInts() : (Int, Int) = readInts() match {
    case Seq(a, b) => (a, b)
  }
}

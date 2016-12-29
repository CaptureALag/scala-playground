package Utils

import scala.io.StdIn

object IO {
  def readInts() : Seq[Int] = StdIn.readLine().split("\\s+").map(_.toInt)
  def readPairOfInts() : (Int, Int) = readInts() match {
    case Seq(a, b) => (a, b)
  }
}

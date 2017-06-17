package cf817

import scala.io.StdIn._

object A extends App {
  def readInts() = readLine() split " " map (_.toInt) toSeq
  val Seq(x1,y1,x2,y2) = readInts()
  val Seq(x, y) = readInts()

  def pathExists(a: Int, b: Int, step: Int) = (b - a) % step == 0
  def pathIsEven(a: Int, b: Int, step: Int) = (b - a) / step % 2 == 0

  println(
    if(pathExists(x1, x2, x) && pathExists(y1, y2, y) && (pathIsEven(x1, x2, x) == pathIsEven(y1, y2, y)))
      "YES"
    else
      "NO"
  )
}
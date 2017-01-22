package cf750.d

import scalaz._, Scalaz._

trait Direction {
  def x : Byte
  def y : Byte
  def cw : Direction
  def ccw : Direction
  def opposite : Direction
}

object Direction {
  private case class DirectionImpl(x : Byte, y : Byte) extends Direction {
    def thiz : DirectionImpl = this

    def cw : Direction =
      ccwStream.sliding(2).find(_.tail.head == this).get.head

    def ccw : Direction =
      ccwStream.sliding(2).find(_.head == this).get.tail.head

    def opposite : Direction =
      ccwStream.sliding(5).find(_.head == this).get.drop(4).head
  }

  private val ccwStream : Stream[Direction] =
    Stream.continually(
      Seq(
        (0, 1),
        (-1, 1),
        (-1, 0),
        (-1, -1),
        (0, -1),
        (1, -1),
        (1, 0),
        (1, 1)
      ).map({ case (x, y) => DirectionImpl(x.toByte, y.toByte) })
    ).flatten

  private val cwStream : Stream[Direction] =
    Stream.continually(ccwStream.take(8).reverse).flatten

  val top : Direction = ccwStream.head

  def iterateCcwStartingFrom(direction : Direction) : Iterator[Direction] =
    ccwStream.dropWhile(_ != direction).iterator

  def iterateCwStartingFrom(direction : Direction) : Iterator[Direction] =
    cwStream.dropWhile(_ != direction).iterator
}

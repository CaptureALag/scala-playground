package cf750.b

import Utils.Bool2String
import Utils.IO._

import scalaz._
import Scalaz._

object SolutionIO extends App {
  val n = readInt()

  val commands = (1 to n).map(_ => {
    val Seq(dist, dir) = readTokens()
    (dist.toInt, Direction.withName(dir))
  })

  val answer = new Solution(commands).answer

  println(Bool2String(answer))
}

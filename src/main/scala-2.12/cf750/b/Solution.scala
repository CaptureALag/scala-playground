package cf750.b

import scalaz._, Scalaz._

class Solution(directions : Seq[(Int, Direction.Value)]) {
  val res =
    directions.foldLeft(\/-(YPosition.initial) : \/[String, YPosition])({
      case (res, (dist, dir)) =>
        res.flatMap(pos => pos.move(dir, dist))
    })

  val answer = res.flatMap(pos => if(pos == YPosition.initial) \/-(pos) else -\/("")).isRight
}

package cf750.b

import scalaz._, Scalaz._
import Direction._

trait YPosition {
  val y : Int
  def move(direction: Direction.Value, distance : Int) : String \/ YPosition
}

object YPosition {

  val equator = 40000
  val halfEquator : Int = equator / 2

  case class YPositionImpl(y : Int) extends YPosition {
    override def move(direction: Direction.Value, distance: Int) : String \/ YPosition = {

      val yValidationConfig : Seq[((Int, Direction.Value), String)] =
        Seq(
          (0 -> South, "North Pole"),
          (halfEquator -> North, "South Pole")
        ).map({
          case ((y, dir), loc) => (y -> dir, s"One can move only $dir from $loc($this)")
        })

      val yValidators = yValidationConfig.map({
        case ((y, dir), failMessage) => {
          case (_y : Int, _direction : Direction.Value) if _y == y && _direction != dir =>
              -\/(failMessage + "; got " + _direction)
          case _ =>
              \/-(())
        } : ((Int, Direction.Value) => String \/ Unit)
      })

      for {
        kek <- yValidators.map(_(this.y, direction)).toList.sequenceU
        newPos <- YPosition(direction match {
          case North => y - distance
          case South => y + distance
          case _ => y
        })
      } yield newPos
    }
  }

  def apply(y : Int): String \/ YPosition =
    if((0 to halfEquator).contains(y))
      \/-(new YPositionImpl(y))
    else
      -\/(s"Y coordinate out of range: $y")

  def initial : YPosition = YPositionImpl(0)
}

package cf750.d

object Solution extends App {
  def apply(t : Int*): Int = {
    val fireworkPoints = ShapeBuilder.buildFireworks(t).points
    fireworkPoints.size
  }
}

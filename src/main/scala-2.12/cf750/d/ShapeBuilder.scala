package cf750.d

object ShapeBuilder {
  private class ShapeBuilder(val points : Set[(Int, Int)]) {
    def rotateCw90 : ShapeBuilder =
      new ShapeBuilder(points.view.map({
        case (x, y) => (y, -x)
      }).toSet)

    def drawLineTo(dx : Byte, dy : Byte, len : Int) : ShapeBuilder =
      new ShapeBuilder(
        Seq(
          (1 to len).map(-_).map(i => (dx * i, dy * i)),
          points.view.map({ case (x, y) => (x - dx * len, y - dy * len) })
        ).flatten.toSet
      )

    def merge(shapeBuilder: ShapeBuilder) =
      new ShapeBuilder(this.points ++ shapeBuilder.points)

    def build : Shape = {
      val builderPts = points
      new Shape {
        val points: Seq[(Int, Int)] = builderPts.toSeq
      }
    }
  }

  private def empty = new ShapeBuilder(Set())

  def buildFireworks(t : Seq[Int]): Shape = {
    val startDir = Direction.iterateCcwStartingFrom(Direction.top).drop(t.size - 1).next.opposite

    Direction.iterateCwStartingFrom(startDir).zip(t.reverseIterator).foldLeft(empty)({
      case (shapeBuilder, (dir, len)) =>
        shapeBuilder.merge(shapeBuilder.rotateCw90).drawLineTo(dir.x, dir.y, len)
    }).build
  }
}

package cf691.d

class Graph(val verticeCount : Int, rawEdges : Seq[(Int, Int)]) {
  val edges : Vector[Seq[Int]] =
    rawEdges.foldLeft(Vector.fill(verticeCount)(Seq.empty[Int]))({
      case (res : Vector[Seq[Int]], (a, b)) =>
        res.updated(a, b +: res(a)).updated(b, a +: res(b))
    }).map(_.reverse)
}

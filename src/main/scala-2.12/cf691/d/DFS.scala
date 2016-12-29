package cf691.d

import scala.collection.immutable.BitSet
import scalaz._, Scalaz._

object DFS extends ((Graph, Int) => Seq[Int]) {

  override def apply(graph: Graph, vertex: Int): Seq[Int] = {
    def _dfs(vertex : Int) : State[BitSet, Seq[Int]] =
      for {
        visitedNodes <- get[BitSet]
        vertexQueue = graph.edges(vertex).filterNot(visitedNodes.contains)
        _ <- put(visitedNodes + vertex)
        dfsQueue = vertexQueue.toList.map(_dfs)
        dfsNext = dfsQueue.sequenceU
        vertexNext <- dfsNext
      } yield vertex +: vertexNext.flatten
    _dfs(vertex).eval(BitSet.empty)
  }
}

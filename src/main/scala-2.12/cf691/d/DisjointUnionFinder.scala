package cf691.d

import scala.collection.immutable.BitSet
import scalaz._, Scalaz._

object DisjointUnionFinder extends (Graph => Seq[Seq[Int]]) {

  override def apply(graph: Graph): Seq[Seq[Int]] = {

    def step(vertex : Int) : State[BitSet, Option[Seq[Int]]] =
      for {
        usedNodes <- get[BitSet]
        union = Some((_ : Any) => DFS(graph, vertex))
                  .filterNot(_ => usedNodes.contains(vertex))
                  .map(_('kek))
        _ <- put(usedNodes ++ union.getOrElse(Seq()))
      } yield union

    (0 until graph.verticeCount)
      .map(step)
      .toList
      .sequenceU
      .eval(BitSet.empty)
      .collect { case Some(x) => x }
  }
}

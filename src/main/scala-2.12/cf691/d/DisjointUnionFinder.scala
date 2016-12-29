package cf691.d

import scala.collection.immutable.BitSet
import scalaz._, Scalaz._

object DisjointUnionFinder extends (Graph => Seq[Seq[Int]]) {

  type BitSetState[T] = State[BitSet, T]
  type OptionTBitSetState[T] = OptionT[BitSetState, T]
  object OptionTBitSetState {
    def apply[T](option : Option[T]) : OptionT[BitSetState, T] =
      OptionT[BitSetState, T](State[BitSet, Option[T]](_ -> option))

    def apply[T](state : State[BitSet, T]) : OptionT[BitSetState, T] =
      OptionT[BitSetState, T](state.map(_.some))
  }


  override def apply(graph: Graph): Seq[Seq[Int]] = {

    def step(vertex : Int) : OptionTBitSetState[Seq[Int]] =
      for {
        usedNodes <- OptionTBitSetState(get[BitSet])
        union <- OptionTBitSetState(
                  Some((_ : Any) => DFS(graph, vertex))
                  .filterNot(_ => usedNodes.contains(vertex))
                  .map(_('kek))
                )

      } yield union

    (0 until graph.verticeCount).map(step).toList.sequenceU.run(BitSet.empty)

    Seq()
  }
}

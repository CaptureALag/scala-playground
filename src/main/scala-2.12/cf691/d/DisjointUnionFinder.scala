package cf691.d

import scala.collection.immutable.BitSet
import scalaz._, Scalaz._

object DisjointUnionFinder extends (Graph => Seq[Seq[Int]]) {

  type BitSetState[T] = State[BitSet, T]

  implicit def liftOpt[T](option : Option[T]) : OptionT[BitSetState, T] =
    OptionT[BitSetState, T](State[BitSet, Option[T]](_ -> option))

  implicit def liftState[T](state : State[BitSet, T]) : OptionT[BitSetState, T] =
    OptionT[BitSetState, T](state.map(_.some))

  override def apply(graph: Graph): Seq[Seq[Int]] = {

    def step(vertex : Int) : OptionT[BitSetState, Seq[Int]] =
      for {
        usedNodes <- liftState(get[BitSet])
        union <- liftOpt(Some((_ : Any) => DFS(graph, vertex)).filterNot(_ => usedNodes.contains(vertex)).map(_('kek)))
      } yield union
    Seq()
  }
}

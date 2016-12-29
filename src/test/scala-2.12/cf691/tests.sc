import scalaz._, Scalaz._
import scala.collection.immutable.BitSet

type BitSetState[T] = State[BitSet, T]
type OptionTBitSetState[T] = OptionT[BitSetState, T]
object OptionTBitSetState {
  def apply[T](option : Option[T]) : OptionT[BitSetState, T] =
    OptionT[BitSetState, T](State[BitSet, Option[T]](_ -> option))

  def apply[T](state : State[BitSet, T]) : OptionT[BitSetState, T] =
    OptionT[BitSetState, T](state.map(_.some))
}

def step(i : Int) : OptionTBitSetState[Seq[Int]] =
  for {
    usedIs <- OptionTBitSetState(get[BitSet])
    res <- OptionTBitSetState(
      Some(Seq(i, i*10, i*100)).filterNot(_ => usedIs.contains(i))
    )
    _ <- OptionTBitSetState(put(usedIs + i))
  } yield res

List(1,2,1,3).map(step).sequence.run(BitSet.empty)._2
package cf750.e

import rx.Single.OnSubscribe
import rx.SingleSubscriber
import rx.lang.scala.{Observable, Observer}

import scalaz._
import Scalaz._
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap

class _201Dropper(val s : String)  {

  private val _012Pos : Map[Char, SortedSet[Int]] =
    s.view.zipWithIndex
      .filter(Seq('2','0','1') contains _._1)
      .groupBy(_._1)
      .mapValues(xs => SortedSet(xs.map(_._2) : _*))
      .withDefaultValue(SortedSet.empty)

  private val _201Intervals = SortedMap[Int, Int](
    _012Pos('2').view
    .collect(Function.unlift(a => _012Pos('0').iteratorFrom(a).find(_ => true).map(a -> _)))
    .collect(Function.unlift({ case (a, b) => _012Pos('1').iteratorFrom(b).find(_ => true).map(a -> _) }))
    .toSeq : _*
  )

  println(_201Intervals)

  def processSubstring(a : Int, b : Int) : Observable[Option[(Int, Int)]] =
    Observable.just(
      _201Intervals.valuesIteratorFrom(a).find(_ => true).flatMap({
        case end if end < b => Some(end + 1, b)
        case _ => None
      })
    )

}

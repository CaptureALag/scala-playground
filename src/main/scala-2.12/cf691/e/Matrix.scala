package cf691.e

import scalaz._
import Scalaz._
import scala.collection.immutable.BitSet

class Matrix[T](val rows : Iterable[Iterable[T]])(implicit num : Numeric[T], mod : Int = 1000000007) {
  private val numModVal = num.fromInt(mod)
  private def numMod(a : T, b : T) : T =
    Stream.iterate(a)(num.minus(_, b))
      .grouped(2).map({ case Stream(x, y) => (x, y)})
      .find(x => num.compare(x._2, num.zero) < 0)
      .get._1
  private def normalizeNum : T => T = numMod(_, numModVal)

  val columns : Iterable[Iterable[T]] = rows.transpose

  private class IdMatrix(matrixSize : Int) extends Matrix[T](Stream.range(0, matrixSize).map(i => (0 until matrixSize).map(j => if(i == j) num.one else num.zero))) {
    override def sum: T = num.fromInt(matrixSize)
  }
  lazy val idMatrix : Matrix[T] = new IdMatrix(rows.size)

  def multiply(other : Matrix[T]) : Matrix[T] = {
    require(this.columns.size == other.rows.size)
    (this, other) match {
      case (m, _ : IdMatrix) => m
      case (_ : IdMatrix, m) => m
      case _ =>
        new Matrix(
          this.rows.map(other.columns.toStream.strengthL(_).map({
            case (x, y) =>
              val sum = x.toStream.fzipWith(y.toStream)(num.times).reduce(num.plus)
              normalizeNum(sum)
          }))
        )
    }
  }

  def square : Matrix[T] = this.multiply(this)



  type MatrixState[R] = State[Matrix[T], R]
  type MatrixMultiplicatorTMatrixState[R] = WriterT[MatrixState, Matrix[T], R]
  object MatrixMultiplicatorTMatrixState {
    def apply[R](fState : Matrix[T] => (Matrix[T], R), toMultiply : Matrix[T] = multiplicationMonoid.zero) : MatrixMultiplicatorTMatrixState[R] =
      WriterT[MatrixState, Matrix[T], R](State[Matrix[T], (Matrix[T], R)](
        fState.andThen({
          case (st, r) => st -> (toMultiply, r)
        })))

    def idState : Matrix[T] => (Matrix[T], Unit) = (identity[Matrix[T]] _).andThen((_, ()))

    def lift[R](state : MatrixState[R]) : MatrixMultiplicatorTMatrixState[R] =
      apply(state.run)

    def multiplyWriterBy(matrix : Matrix[T]) : MatrixMultiplicatorTMatrixState[Unit] =
      apply(idState, matrix)

    def doNothing : MatrixMultiplicatorTMatrixState[Unit] =
      apply(idState)

    def If(cond : Boolean, action : MatrixMultiplicatorTMatrixState[Unit]) : MatrixMultiplicatorTMatrixState[Unit] =
      if(cond)
        action
      else
        doNothing

    private class MultiplicationMonoid(matrixSize : Int) extends Monoid[Matrix[T]] {

      override def zero: Matrix[T] = idMatrix

      override def append(m1: Matrix[T], m2: => Matrix[T]): Matrix[T] = {
        val res = m1.multiply(m2)
        // print(s"appending ${m1.sum} ${m2.sum} = ${res.sum} | ")
        res
      }
    }

    implicit def multiplicationMonoid : Monoid[Matrix[T]] = {
      new MultiplicationMonoid(rows.size)
    }

  }

  def power : Long => Matrix[T] = {
    case 0 => idMatrix
    case pow if pow > 0 =>
      val powsOf2 = BitSet.fromBitMaskNoCopy(Array(pow))
      val maxPow = powsOf2.lastKey
      import MatrixMultiplicatorTMatrixState._
      def _power(bit : Int) : MatrixMultiplicatorTMatrixState[Unit] =
        for {
          poweredSoFar <- lift(get[Matrix[T]])
          _ <- If(powsOf2.contains(bit), multiplyWriterBy(poweredSoFar))
          _ <- If(bit != maxPow, lift(modify[Matrix[T]](_.square)))
        } yield ()
      val res = Stream.range(0, maxPow + 1).map(_power).sequence.run(this)
      //println(s"res - ${res._1.sum}, ${res._2._1.sum}")
      res._2._1
  }

  def sum : T = rows.map(_.reduce(num.plus)).reduce(num.plus)
}

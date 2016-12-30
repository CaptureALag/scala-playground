package cf691.e

import scalaz._
import Scalaz._
import scala.collection.immutable.BitSet

class Matrix[T](val rows : Iterable[Iterable[T]])(implicit num : Numeric[T]) {
  val columns : Iterable[Iterable[T]] = rows.transpose

  def multiply(other : Matrix[T]) : Matrix[T] = {
    require(this.columns.size == other.rows.size)
    new Matrix(
      this.rows.map(other.columns.toStream.strengthL(_).map({
        case (x, y) => x.toStream.fzipWith(y.toStream)(num.times).reduce(num.plus)
      }))
    )
  }

  def square : Matrix[T] = this.multiply(this)



  type MatrixState[R] = State[Matrix[T], R]
  type MatrixMultiplicatorTMatrixState[R] = WriterT[MatrixState, Matrix[T], R]
  object MatrixMultiplicatorTMatrixState {
    def apply[R](f : Matrix[T] => (Matrix[T], (Matrix[T], R))) : MatrixMultiplicatorTMatrixState[R] =
      WriterT[MatrixState, Matrix[T], R](State[Matrix[T], (Matrix[T], R)](f))

    def modifyState(f : Matrix[T] => Matrix[T]) : MatrixMultiplicatorTMatrixState[Matrix[T]] =
      apply(st => {
        val newSt = f(st)
        multiplicationMonoid.zero -> (newSt, newSt)
      })

    def multiplyWriterBy(matrix : Matrix[T]) : MatrixMultiplicatorTMatrixState[Unit] =
      apply(st => matrix -> (st, ()))

    def doNothing : MatrixMultiplicatorTMatrixState[Unit] =
      apply(st => multiplicationMonoid.zero -> (st, ()))

    def If(cond : Boolean, action : MatrixMultiplicatorTMatrixState[Unit]) : MatrixMultiplicatorTMatrixState[Unit] =
      if(cond)
        action
      else
        doNothing

    private class MultiplicationMonoid(matrixSize : Int) extends Monoid[Matrix[T]] {
      private object IdMatrix extends Matrix[T](Stream.range(1, matrixSize).map(i => (1 to matrixSize).map(j => if(i == j) num.one else num.zero)))

      override def zero: Matrix[T] = IdMatrix

      override def append(m1: Matrix[T], m2: => Matrix[T]): Matrix[T] =
        if(m1 == IdMatrix)
          m2
        else
          m1.multiply(m2)
    }

    implicit def multiplicationMonoid : Monoid[Matrix[T]] = {
      new MultiplicationMonoid(rows.size)
    }

  }

  def power(pow : Long) : Matrix[T] = {
    val powsOf2 = BitSet.fromBitMask(Array(pow))
    import MatrixMultiplicatorTMatrixState._
    def _power(bit : Int) : MatrixMultiplicatorTMatrixState[Unit] =
      for {
        squaredMatrix <- modifyState(_.square)
        _ <- If(powsOf2.contains(bit), multiplyWriterBy(squaredMatrix))
      } yield ()
    Stream.range(1, powsOf2.lastKey).map(_power).sequence[MatrixMultiplicatorTMatrixState, Unit].run(this)._1
  }

  def sum : T = rows.map(_.reduce(num.plus)).reduce(num.plus)
}

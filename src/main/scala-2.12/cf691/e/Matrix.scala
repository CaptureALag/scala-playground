package cf691.e

import scalaz._
import Scalaz._
import scala.collection.immutable.BitSet

class Matrix[T](val rows : Iterable[Iterable[T]])(implicit num : Numeric[T]) {
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
            case (x, y) => x.toStream.fzipWith(y.toStream)(num.times).reduce(num.plus)
          }))
        )
    }
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

    def lift[R](state : MatrixState[R]) : MatrixMultiplicatorTMatrixState[R] =
      apply(state.map(multiplicationMonoid.zero -> _).run)

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

      override def zero: Matrix[T] = idMatrix

      override def append(m1: Matrix[T], m2: => Matrix[T]): Matrix[T] = m1.multiply(m2)
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
      Stream.range(0, maxPow).map(_power).sequence.run(this)._1
  }

  def sum : T = rows.map(_.reduce(num.plus)).reduce(num.plus)
}

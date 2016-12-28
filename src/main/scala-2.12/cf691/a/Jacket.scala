package cf691.a

class Jacket(val buttons : Seq[JacketButton]) {
  def isGood : Boolean =
    buttons match {
      case List(x) => x.isInstanceOf[ClosedJacketButton]
      case xs => xs.count(_.isInstanceOf[OpenJacketButton]) == 1
    }
}

object Jacket {
  def parse(s : String) : Jacket =
    new Jacket(
      s.split(' ').map(JacketButton.parse)
    )
}

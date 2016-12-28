package cf691.a

sealed abstract class JacketButton

class OpenJacketButton extends JacketButton

class ClosedJacketButton extends JacketButton

object JacketButton {
  def parse(any : Any) : JacketButton = any match {
    case 1 => new ClosedJacketButton
    case 0 => new OpenJacketButton
    case s : String => parse(s.toInt)
  }
}
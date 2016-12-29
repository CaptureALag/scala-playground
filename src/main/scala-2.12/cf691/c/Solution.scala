package cf691.c

import scala.io.StdIn
import scalaz._

object Solution extends App {
  println(SimpleExpFormatter(StdIn.readLine()).getOrElse("Error: " ++ (_ : String)))
}

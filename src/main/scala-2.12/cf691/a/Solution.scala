package cf691.a

import Utils.Bool2String

import scala.io.StdIn

object Solution extends App {
  StdIn.readLine()
  println(
    Bool2String.apply(Jacket.parse(StdIn.readLine()).isGood)
  )
}

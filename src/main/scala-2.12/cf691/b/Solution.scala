package cf691.b

import Utils.Bool2String

import scala.io.StdIn

object Solution extends App {
  println(Bool2String(SPalyndromeChecker(StdIn.readLine()))("TAK", "NIE"))
}

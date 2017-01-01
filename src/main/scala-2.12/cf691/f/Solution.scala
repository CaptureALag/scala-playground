package cf691.f

import Utils.IO._

class Solution(a : Seq[Int], p : Seq[Int]) {
  val queryAnswerer = new CoupleCoverQueryAnswerer(a, p.max)

  val answers : Seq[Int] = p.map(queryAnswerer.apply)
}

object Solution extends App {
  readString()
  val a = readInts()
  readString()
  val p = readInts()

  println(new Solution(a, p).answers.mkString("\n"))
}
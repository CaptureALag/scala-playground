package cf750.a

import TimeToParty._, NumberOfTasks._

class Solution(numberOfTasks: NumberOfTasks, timeToParty: TimeToParty) {
  println((1 to numberOfTasks).scanLeft(0)(_ + _))
  val answer : Int = (1 to numberOfTasks).scanLeft(0)(_ + _).tail.takeWhile(_ * 5 + timeToParty <= 240).size
}


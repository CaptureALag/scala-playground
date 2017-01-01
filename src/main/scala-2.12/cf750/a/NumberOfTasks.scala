package cf750.a

case class NumberOfTasks(value : Int) extends AnyVal

object NumberOfTasks {
  implicit def wrap(value : Int) : NumberOfTasks = NumberOfTasks(value)
  implicit def unwrap(numberOfTasks: NumberOfTasks) : Int = numberOfTasks.value
}

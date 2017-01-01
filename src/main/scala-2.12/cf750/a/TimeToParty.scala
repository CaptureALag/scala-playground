package cf750.a

case class TimeToParty(value : Int) extends AnyVal

object TimeToParty {
  implicit def wrap(value : Int) : TimeToParty = TimeToParty(value)
  implicit def unwrap(timeToParty: TimeToParty) : Int = timeToParty.value
}

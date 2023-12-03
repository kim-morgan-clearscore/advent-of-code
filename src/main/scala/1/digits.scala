package dayOne

sealed trait Digit {
  def value: Int
  def representations: List[String]
}
case object One extends Digit {
  val value = 1
  val representations = List("1", "one")
}
case object Two extends Digit {
  val value = 2
  val representations = List("2", "two")
}
case object Three extends Digit {
  val value = 3
  val representations = List("3", "three")
}
case object Four extends Digit {
  val value = 4
  val representations = List("4", "four")
}
case object Five extends Digit {
  val value = 5
  val representations = List("5", "five")
}
case object Six extends Digit {
  val value = 6
  val representations = List("6", "six")
}
case object Seven extends Digit {
  val value = 7
  val representations = List("7", "seven")
}
case object Eight extends Digit {
  val value = 8
  val representations = List("8", "eight")
}
case object Nine extends Digit {
  val value = 9
  val representations = List("9", "nine")
}

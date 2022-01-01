package exercises

object AdvancedPatternMatchingExercise extends App:

  val nomatch = "\\_(ツ)_/¯"
  val n: Int = 45

  // solve this via pattern matching
  val matchedProperty = n match
    case x if x < 10 => "single digit"
    case x if x % 2 == 0 => "an even number"
    case _ => nomatch

  println(matchedProperty)

  object SingleDigit:
    def unapply(n: Int): Option[Int] =
      if n < 10 then Some(n) else None

  object EvenNumber:
    def unapply(n: Int): Option[Int] =
      if n % 2 == 0 then Some(n) else None

  val matchedProperty2 = n match
    case SingleDigit(x) => "single digit"
    case EvenNumber(x) => "an even number"
    case _ => nomatch

  println(matchedProperty2)

  object Property:
    def unapply(n: Int): Option[String] =
      if n < 10 then Some("single digit")
      else if n % 2 == 0 then Some("an even number")
      else None

  val matchedProperty3 = n match
    case Property(p) => p
    case _ => nomatch

  println(matchedProperty3)

  // Daniel's solution:
  object even:
    def unapply(n: Int): Boolean =
      n % 2 == 0

  object singleD:
    def unapply(n: Int): Boolean =
      n > -10 && n < 10

  val danielled = n match
    case singleD() => "single digit"
    case even() => "an even number"
    case _ => nomatch

  println(danielled)

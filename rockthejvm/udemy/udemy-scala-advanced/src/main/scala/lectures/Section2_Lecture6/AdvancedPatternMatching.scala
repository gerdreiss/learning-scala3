package lectures.Section2_Lecture6

object AdvancedPatternMatching extends App:

  val numbers = List(1)
  val description = numbers match
    case head :: Nil => s"the only element is $head"
    case _ => "unclear situation"

  println(description)

  /** Pattern matchable things are:
    *   - constants
    *   - wildcards
    *   - case classes
    *   - tuples
    *   - some special magic like above
    */
  class Person(val name: String, val age: Int)
  object Person:
    def unapply(p: Person): Option[(String, Int)] = Some((p.name, p.age))
    // and now for something crazy
    def unapply(age: Int): Option[String] =
      Some(if age < 21 then "minor" else "major")

  val bob = Person("Bob", 42)
  val greeting = bob match
    case Person(n, a) => s"Name: $n, age: $a"

  println(greeting)

  val legalStatus = bob.age match
    case Person(status) => s"Legal status: $status"

  println(legalStatus)

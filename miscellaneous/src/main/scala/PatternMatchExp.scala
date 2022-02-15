case class Person(name: String, age: Int, gender: Int)

object Adult:
  def unapply(age: Int): Boolean = age > 18

object Teenager:
  def unapply(age: Int): Boolean = (12 to 18) contains age

object Male:
  def unapply(gender: Int): Boolean = gender == 1

object Female:
  def unapply(gender: Int): Boolean = gender == 2

object Child:
  def unapply(p: Person): Boolean = p.age < 12

object PatternMatchExp extends App:

  Person("john doe", 2, 1) match
    case Person(_, Adult(), Male())      => println("adult male")
    case Person(_, Teenager(), Female()) => println("female teenager")
    case Child()                         => println("child")
    case _                               => println("somebody else")

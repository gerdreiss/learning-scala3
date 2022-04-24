case class Person(name: String, age: Int, gender: Int)

/** Imperative solution */
def imperative(p: Person): Unit =
  if p.gender == 1 && p.age > 18 then println(s"${p.name}, aged ${p.age}, is in college")
  else if p.gender == 2 && p.age >= 12 && p.age <= 18 then println(s"${p.name}, aged ${p.age}, is in high school")
  else if p.age < 12 then println(s"${p.name} is in primary school")
  else println("go to work, bum!")

/** Functional Scala solution */
object Adult:
  def unapply(age: Int): Option[Int] =
    Option.when(age > 18)(age)

object Teenager:
  def unapply(age: Int): Option[Int] =
    Option.when((12 to 18) contains age)(age)

object Male:
  def unapply(gender: Int): Boolean =
    gender == 1

object Female:
  def unapply(gender: Int): Boolean =
    gender == 2

object Child:
  def unapply(p: Person): Option[String] =
    Option.when(p.age < 12)(p.name)

@main def PatternMatchExp(): Unit =
  Person("john doe", 21, 1) match
    case Person(name, Adult(age), Male())      => println(s"adult male $name aged $age")
    case Person(name, Teenager(age), Female()) => println(s"female teenager $name aged $age")
    case Child(name)                           => println(s"child $name")
    case _                                     => println("somebody else")

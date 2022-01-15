package lectures.section2

object AdvancedPatternMatching extends App:

  val nomatch = """¯\_(ツ)_/¯"""

  val numbers = List(1)
  val description = numbers match
    case head :: Nil => s"the only element is $head"
    case _ => nomatch

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

  // infix patterns
  case class Or[A, B](a: A, b: B) // Either
  val either = Or(2, "two")
  val humanDesc = either match
    case n Or s => s"$n written as '$s'"
  println(humanDesc)

  // decomposing sequences
  val vararg = numbers match
    case List(1, _*) => "starting with one"
    case _ => nomatch

  println(vararg)

  abstract class MyList[+A]:
    def head: A = ???
    def tail: MyList[A] = ???

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList:
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if list == Empty then Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match
    case MyList(1, 2, _*) => "starting with 1 and 2"
    case _ => nomatch

  println(decomposed)

  // custom return types for unapply
  // the custom type needs to have these methods:
  // def isEmpty: Boolean
  // def get: A

  abstract class Wrapper[T]:
    def isEmpty: Boolean
    def get: T

  object PersonWrapper:
    def unapply(p: Person): Wrapper[String] =
      new:
        override def isEmpty: Boolean = false
        override def get: String = p.name

  val z = bob match
    case PersonWrapper(n) => s"Name = $n"
    case _ => nomatch

  println(z)

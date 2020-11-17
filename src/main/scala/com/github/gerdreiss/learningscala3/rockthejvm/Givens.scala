package com.github.gerdreiss
package learningscala3
package rockthejvm

// census
case class Person(
    surname: String,
    name: String,
    age: Int
  )

object Givens extends App:

  val people      = List(Person("Doe", "John", 50),Person("Doe", "Jane", 50))
  val maybePeople = people.map(Option(_))

  // use everywhere
  val personOrdering: Ordering[Person] = new Ordering[Person] {
    override def compare(a: Person, b: Person): Int =
      a.surname.compareTo(b.surname)
  }

  def listPeople(
    person: Seq[Person]
  )(
    ordering: Ordering[Person] // <--explicit
  ): Seq[Person] = ???

  def someOtherFunctionRequiringOrdering(
    person: Seq[Person]
  )(
    ordering: Ordering[Person] // <--explicit
  ): Int = ???

  listPeople(people)(personOrdering)
  someOtherFunctionRequiringOrdering(people)(personOrdering)

  // find the "standard" value
  // explicitly call methods

  // given/using
  // given standardPersonOrdering as Ordering[Person]:
  //   override def compare(a: Person, b: Person): Int =
  //     a.surname.compareTo(b.surname)

  def someFunctionRequiringStandardOrdering(
      persons: List[Person]
    )(
      using ordering: Ordering[Person] // <-- implicit
    ): List[Person] = ???
  
  // import givens
  // 1. import explicitly
  import StandardValues.standardPersonOrdering

  // 2. import given for a TYPE (the only one)
  import StandardValues.{given Ordering[Person]}

  // 3. import all the givens
  import StandardValues.{given _}

  // this does not import givens
  import StandardValues._

  someFunctionRequiringStandardOrdering(people)

  def sortThings[T](things: List[T])(using ordering: Ordering[T]) = ???

  sortThings(maybePeople)


// contains the actual givens
object StandardValues:

  // given/using

  given standardPersonOrdering as Ordering[Person]:
    override def compare(a: Person, b: Person): Int =
      a.surname.compareTo(b.surname)

  // deriving givens
  // working with Options
  // create a given Ordering[Option[T]] ifwe had an Ordering[T] in scope

  given optionOrdering[T](using tOrdering: Ordering[T]) as Ordering[Option[T]]:
    override def compare(maybeA: Option[T], maybeB: Option[T]): Int =
     (maybeA, maybeB) match
      case (None, None) => 0
      case (None, _)    => -1
      case (_, None)    => 1
      case (Some(a), Some(b)) => tOrdering.compare(a, b)

package lectures.section5

case class Person(name: String, age: Int)

trait HtmlSerializer[T]:
  def serialize(value: T): String

object HtmlSerializer:
  def serialize[T](value: T)(using serializer: HtmlSerializer[T]): String =
    serializer.serialize(value)

  def apply[T](using serializer: HtmlSerializer[T]) = serializer

given HtmlSerializer[Person] with
  def serialize(p: Person): String = s"<p>Name: ${p.name}, age: ${p.age}</p>"

extension (p: Person)
  def toHtml(using serializer: HtmlSerializer[Person]): String =
    serializer.serialize(p)

@main def testTypeClass(): Unit =
  println(HtmlSerializer.serialize(Person("Joe", 40)))
  println(Person("Joe", 40).toHtml)

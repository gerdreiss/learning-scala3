object ExtensionMethods extends App:
  // type enrichment == "pimping"
  case class Person(name: String):
    def greet: String = s"Hi, my name is $name"

  // Scala 2 way:
  implicit class PersonLike(name: String):
    def introduce: String = Person(name).greet

  val greeting = "G".introduce // new PersonLike("G").introduce

  // Scala 3 way:

  extension (s: String)
    def introExt: String   = Person(s).greet
    def numberOfChars: Int = s.toLowerCase.nn.groupBy(identity).size

  println("G".introExt)

  // comes from a library

  sealed abstract class Tree[+A]
  case class Leaf[+A](value: A)                        extends Tree[A]
  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // I need to extend tree functionality

  extension [A](tree: Tree[A])
    def exists(predicate: A => Boolean): Boolean =
      tree match
        case Leaf(a)             => predicate(a)
        case Branch(left, right) => left.exists(predicate) || right.exists(predicate)

    def map[B](f: A => B): Tree[B] =
      tree match
        case Leaf(a)             => Leaf(f(a))
        case Branch(left, right) => Branch(left.map(f), right.map(f))

    def sum(using numeric: Numeric[A]): A =
      tree match
        case Leaf(a)             => a
        case Branch(left, right) => numeric.plus(left.sum, right.sum)

  val tree = Branch(Leaf(1), Leaf(2))
  println(tree.exists(_ > 0))
  println(tree.map(_ * 10))
  println(tree.sum)

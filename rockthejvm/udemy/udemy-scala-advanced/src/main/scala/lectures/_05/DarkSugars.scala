package lectures._05

import scala.util.Try

class DarkSugars extends App:

  // syntax sugar #1: methods with single param can be called with {} instead of ()
  def singleParamFunction(p: Int): String = s"$p little ducks"

  val desc = singleParamFunction {
    // write some complex code
    42
  }

  val aTryInstance = Try {
    throw new Exception("Boom!")
  }

  List(1, 2, 3).map { x =>
    x + 1
  }

  // syntax sugar #2: single method traits can be used as a lambda expression
  trait Action:
    def act(s: String): String

  val action: Action = (s: String) => s.zip(s).mkString

  // syntax sugar #3: the :: and #:: methods are special
  val prependedList = 2 :: List(3, 4)
  // 2.::(List(3, 4))
  // List(3, 4).::(2)
  // ?!

  // scala spec: last char decides associativity of method
  // ends with `:` -> right associative
  val truth = 1 :: 2 :: List(3, 4) == List(3, 4).::(2).::(1)

  // example
  class MyStream[T]:
    def -->:(value: T): MyStream[T] = this // actual implementation here

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4: multi-word function naming
  class TeenGurl(name: String):
    def `and then said`(gossip: String): Unit =
      println(s"$name said '$gossip'")

  val lilly = TeenGurl("Lilly")
  lilly `and then said` "Scala is so sweet!"

  // syntax sugar #5: infix types
  class Composite[A, B]
  val composite: Composite[Int, String] = new Composite[Int, String]
  val infixType: Int Composite String = new Composite[Int, String]

  class -->[A, B]
  val towards: Int --> String = new -->[Int, String]

  // syntax sugar #6: update() is very special, much like apply()

  val arr = Array(1, 3, 5)
  arr(2) = 7 // rewritten to arr.update(2, 7)
  // used in mutable collections
  // remember apply() and update()

  // syntax sugar #7: setters for mutable containers
  class Mutable:
    private var internalMember: Int = 0
    // getter
    def member: Int = internalMember
    // setter
    def member_=(value: Int): Unit = internalMember = value

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // rewritten as aMutableContainer.member_=(42)

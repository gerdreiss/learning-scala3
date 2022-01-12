package lectures.section5

class OuterClass:
  class InnerClass
  object InnerObject
  type InnerType

  def print(i: InnerClass) = println(i)
  def printGeneral(i: OuterClass#InnerClass) = println(i)

def aMethod: Int =
  class HelperClass
  type HelperType = String
  42

@main def PathDependentTypes(): Unit =
  // per-instance
  val outer = new OuterClass
  val inner = new outer.InnerClass

  val outer2 = new OuterClass
  val inner2 = new outer2.InnerClass

  outer.print(inner)
  outer.printGeneral(inner2)

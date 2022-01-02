package lectures.section3

object LazyEvaluation extends App:
  lazy val x: Int =
    println("evaluating x...")
    42

  println(x) // "evaluating x..." printed
  println(x) // "evaluating x..." not printed

  def sideEffectCondition: Boolean =
    println("side effecting...")
    true

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  // no is not printed, coz simpleCondition only is evaluated
  println(if simpleCondition && lazyCondition then "yes" else "no")

  def byNameMethod(n: => Int): Int =
    lazy val t = n
    t + t + t + 1

  def retrieveMagicValue: Int =
    println("waiting...")
    // side effect or a long computation
    Thread.sleep(1000)
    42

  // a def is evaluated every time it's called
  println(byNameMethod(retrieveMagicValue))
  println("-" * 50)

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean =
    println(s"$i is less than 30? ")
    i < 30

  def greaterThan20(i: Int): Boolean =
    println(s"$i is greater than 20?")
    i > 20

  val numbers = List(1, 2, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30) // List(1, 2, 25, 5, 23)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)
  println("-" * 50)

  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20lazy = lt30lazy.withFilter(greaterThan20) // lazy filtering
  gt20lazy.foreach(println)

  // for-comprehensions use withFilter
  for
    a <- List(1, 2, 3)
    if a % 2 == 0
  yield a + 1
  // translates to
  List(1, 2, 3).withFilter(_ % 2 == 0).map(_ + 1)

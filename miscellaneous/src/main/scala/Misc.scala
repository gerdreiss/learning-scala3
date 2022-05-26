import scala.util.Random
case class Config(a: Boolean, b: Boolean, c: Boolean)

@main def Misc(): Unit =

  val x = 1
  val y = 10

  // println(1 << 10)
  // println(1024 * 1024)
  // println(1 << 20)

  import Debug.*
  // debugSingle(x << y)
  // debug("A", x, "B", y)

  // everything in scala is an expression
  var v = 10
  val w =
    while v > 0 do
      v = v - 1
      debug(v)
  debug(w)

  // some more pattern matching coz it's so cool
  val config = Config(Random.nextBoolean, Random.nextBoolean, Random.nextBoolean)

  val d = config match
    case Config(true, true, true)  => debug("All true")
    case Config(true, true, _)     => debug("All true, but not c")
    case Config(true, false, true) => debug("All true, but not b")
    case Config(true, false, _)    => debug("All false, but not a")
    case Config(false, true, true) => debug("All true, but not a")
    case Config(false, true, _)    => debug("All true, but not a")
    case Config(false, _, _)       => debug("All false")
    case _                         => debug("All false")

  debug(d)

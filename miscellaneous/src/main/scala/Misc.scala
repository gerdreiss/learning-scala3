@main def Misc(): Unit =

  val x = 1
  val y = 10

  println(1 << 10)
  println(1024 * 1024)
  println(1 << 20)

  import Debug.*
  debugSingle(x << y)
  debug("A", x, "B", y)

  // everything in scala is an expression
  var v = 10
  val w =
    while v > 0 do
      v = v - 1
      debug(v)
  debug(w)

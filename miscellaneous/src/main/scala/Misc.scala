@main def Misc(): Unit =

  val x = 1
  val y = 10

  println(1 << 10)
  println(1024 * 1024)
  println(1 << 20)

  import Debug.*
  debugSingle(x << y)
  debug("A", x, "B", y)

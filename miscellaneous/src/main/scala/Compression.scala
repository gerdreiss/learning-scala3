import scala.annotation.tailrec

/**
 * Let's compress the given string in the most easy way possible: every time we find a repeated sequence of characters, 
 * we'll just print out the first occurrence of the character plus the number of times it is being shown in this repeated sequence ("aaa" should be converted to "a3").
 */
object Compression extends App:

  def compress(input: String, acc: String = ""): String =
    @tailrec
    def go(acc: String, rest: String): String =
      if rest.isEmpty then acc
      else
        val ch     = rest.head
        val length = rest.takeWhile(_ == ch).length
        if length == 1 then go(acc + ch, rest.tail)
        else go(acc + s"$ch$length", rest.drop(length))

    go("", input)

  def compress2(input: String): String =
    if input.isEmpty then input
    else
      input.tail
        .foldLeft(List((input.head, 1))) { //
          case (Nil, current)                       =>
            (current, 1) :: Nil
          case (acc @ (last, cnt) :: rest, current) =>
            if last == current then (current, cnt + 1) :: rest
            else (current, 1) :: acc
        }
        .map { //
          case (ch, cnt) =>
            if cnt == 1 then s"$ch"
            else s"$ch$cnt"
        }
        .reverse
        .mkString

  println(compress("aaabcccddaa"))  // should print "a3bc3d2a2"
  println(compress2("aaabcccddaa")) // should print "a3bc3d2a2"
  println(compress("a"))
  println(compress2("a"))
  println(compress("a222"))
  println(compress2("a222"))

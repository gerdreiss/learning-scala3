package strings

import scala.annotation.tailrec

@tailrec
def hasValidParensG(s: String): Boolean =
  val r = s.replace("()", "")
  // r match
  //    case ""          => true
  //    case r if r == s => false
  //    case r           => hasValidParensG(r)
  r.isEmpty || r != s && hasValidParensG(r)

// ()   = true
// ()() = true
// (()) = true
// )(   = false
// Complexity: O(n)
def hasValidParensDan(s: String): Boolean =
  @tailrec
  def rec(remaining: String, openParens: Int = 0): Boolean =
    if remaining.isEmpty then openParens == 0
    else if openParens == 0 && remaining.head == ')' then false
    else if remaining.head == '(' then rec(remaining.tail, openParens + 1)
    else rec(remaining.tail, openParens - 1)

  rec(s)

def generateParens(n: Int): List[String] =
  @tailrec
  def rec(rem: Int, acc: Set[String] = Set("()")): Set[String] =
    if rem == 0 then acc
    else
      val newStrings = for
        s   <- acc
        idx <- s.indices
      yield
        val (before, after) = s.splitAt(idx)
        s"$before()$after"

      rec(rem - 1, newStrings)

  if n == 0 then List.empty
  else rec(n).toList

object ValidParentheses extends App:

  println(hasValidParensG("(())"))
  println(hasValidParensG("()(()"))

  println(hasValidParensDan("(())"))
  println(hasValidParensDan("()(()"))

  println(generateParens(1))
  println(generateParens(2))
  println(generateParens(3))
  println(generateParens(10))

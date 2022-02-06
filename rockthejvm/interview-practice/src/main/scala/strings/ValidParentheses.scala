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

object ValidParentheses extends App:

  println(hasValidParensG("(())"))
  println(hasValidParensG("()(()"))

  println(hasValidParensDan("(())"))
  println(hasValidParensDan("()(()"))

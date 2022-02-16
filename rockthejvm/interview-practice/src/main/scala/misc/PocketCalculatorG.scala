package misc

import scala.util.parsing.combinator.*

object PocketCalculatorG extends JavaTokenParsers:

  def number: Parser[BigDecimal] = floatingPointNumber ^^ BigDecimal.apply

  def factor: Parser[BigDecimal] = number | "(" ~> expr <~ ")"

  def term: Parser[BigDecimal] =
    factor ~ rep(
      "*" ~ log(factor)("Mult term") |
        "/" ~ log(factor)("Div term") |
        "^" ~ log(factor)("Pow term")
    ) ^^ { case number ~ list =>
      (number /: list) { // this could also be solved via list.foldleft(number) { ... } - see below
        case (x, "*" ~ y) => x * y
        case (x, "/" ~ y) => x / y
        case (x, "^" ~ y) => x.pow(y.intValue)
      }
    }

  def expr: Parser[BigDecimal] =
    term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
      case number ~ list =>
        list.foldLeft(number) { // same as above, using alternate name for /:
          case (x, "+" ~ y) => x + y
          case (x, "-" ~ y) => x - y
        }
    }

  def apply(input: String): BigDecimal =
    parseAll(expr, input) match
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)

@main def calculator(): Unit =
  println(PocketCalculatorG("(-1.25 * 3.05 + 8.5 / 2.75 ^ 2 + 17.5) * 1233.5").formatted("%.2f"))

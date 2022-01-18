// expression problem ==================================================
package basics

object ExpressionProblem:
  trait Expr
  case class B(boolean: Boolean)          extends Expr
  case class Or(left: Expr, right: Expr)  extends Expr
  case class And(left: Expr, right: Expr) extends Expr
  case class Not(expr: Expr)              extends Expr

  val aGiantBoolean: Expr = Or(And(B(true), B(false)), B(false))

  def eval(expr: Expr): Boolean =
    expr match
      case B(b)      => b
      case Or(a, b)  => eval(a) || eval(b)
      case And(a, b) => eval(b) && eval(b)
      case Not(e)    => !eval(e)

  // include ints
  case class I(int: Int)                  extends Expr
  case class Sum(left: Expr, right: Expr) extends Expr

  def eval2(expr: Expr): Boolean | Int =
    expr match
      case B(b)      => b
      case Or(a, b)  => eval(a) || eval(b)
      case And(a, b) => eval(b) && eval(b)
      case Not(e)    => !eval(e)
      case I(i)      => i
      // and here we have a problem - we have to cast
      case Sum(a, b) => eval(a).asInstanceOf[Int] + eval(b).asInstanceOf[Int]

end ExpressionProblem

// Solution with Tagging ==================================================
object Tagging:
  trait Expr(val tag: String)
  case class B(boolean: Boolean)          extends Expr("bool")
  case class Or(left: Expr, right: Expr)  extends Expr("bool")
  case class And(left: Expr, right: Expr) extends Expr("bool")
  case class Not(expr: Expr)              extends Expr("bool")
  case class I(int: Int)                  extends Expr("int")
  case class Sum(left: Expr, right: Expr) extends Expr("int")

  // solves the problem, but is ugly af
  def eval(expr: Expr): Boolean | Int =
    expr match
      case I(i)      => i
      case B(b)      => b
      case Or(a, b)  =>
        if a.tag == "bool" && b.tag == "bool" then
          eval(a).asInstanceOf[Boolean] || eval(b).asInstanceOf[Boolean]
        else throw java.lang.IllegalArgumentException("invalid argument type")
      case And(a, b) =>
        if a.tag == "bool" && b.tag == "bool" then
          eval(b).asInstanceOf[Boolean] && eval(b).asInstanceOf[Boolean]
        else throw java.lang.IllegalArgumentException("invalid argument type")
      case Not(e)    =>
        if e.tag == "bool" then !eval(e).asInstanceOf[Boolean]
        else throw java.lang.IllegalArgumentException("invalid argument type")

end Tagging

// Solution with Tagless Initial ==================================================
object TaglessInitial:
  trait Expr[A]
  case class B(boolean: Boolean)                            extends Expr[Boolean]
  case class Or(left: Expr[Boolean], right: Expr[Boolean])  extends Expr[Boolean]
  case class And(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
  case class Not(expr: Expr[Boolean])                       extends Expr[Boolean]
  case class I(int: Int)                                    extends Expr[Int]
  case class Sum(left: Expr[Int], right: Expr[Int])         extends Expr[Int]

  def eval[A](expr: Expr[A]): A =
    expr match
      case B(b)      => b
      case Or(a, b)  => eval(a) || eval(b)
      case And(a, b) => eval(b) && eval(b)
      case Not(e)    => !eval(e)
      case I(i)      => i
      // and here we have a problem - we have to cast
      case Sum(a, b) => eval(a) + eval(b)

end TaglessInitial

// Solution with Tagless Final V1 ==================================================
object TaglessFinal:
  trait Expr[A]:
    val value: A // the final value we care about

  def b(b: Boolean): Expr[Boolean] =
    new:
      val value = b

  def i(i: Int): Expr[Int] =
    new:
      val value = i

  def or(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] =
    new:
      val value = left.value || right.value

  def sum(left: Expr[Int], right: Expr[Int]): Expr[Int] =
    new:
      val value = left.value + right.value

  def eval[A](expr: Expr[A]): A = expr.value

end TaglessFinal

// Solution with Tagless Final V2 ==================================================
object TaglessFinalV2:
  trait Algebra[E[_]]:
    def b(b: Boolean): E[Boolean]
    def i(i: Int): E[Int]
    def and(left: E[Boolean], right: E[Boolean]): E[Boolean]
    def or(left: E[Boolean], right: E[Boolean]): E[Boolean]
    def sum(left: E[Int], right: E[Int]): E[Int]

  case class SimpleExpr[A](value: A)
  given Algebra[SimpleExpr] with
    override def b(b: Boolean): SimpleExpr[Boolean]                                              =
      SimpleExpr(b)
    override def i(i: Int): SimpleExpr[Int]                                                      =
      SimpleExpr(i)
    override def and(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] =
      SimpleExpr(left.value && left.value)
    override def or(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean]  =
      SimpleExpr(left.value || right.value)
    override def sum(left: SimpleExpr[Int], right: SimpleExpr[Int]): SimpleExpr[Int]             =
      SimpleExpr(left.value + right.value)

end TaglessFinalV2

import TaglessFinalV2.*

def program1[E[_]](using alg: Algebra[E]): E[Boolean] =
  import alg.*
  or(b(true), and(b(true), b(false)))

def program2[E[_]](using alg: Algebra[E]): E[Int] =
  import alg.*
  sum(i(24), i(-3))

@main def taglessFinalDemo(): Unit =
  println(program1[SimpleExpr])
  println(program2[SimpleExpr])

package misc

import scala.language.experimental

object PocketCalculatorDan extends App:

  val operators = Set("+", "-", "*", "/")

  def eval(expr: String): Int =
    // the defs below could be done like this, but Dan insisted in doing two methods...
    // val (ops, nums) = expr.partition(operators.contains)
    def getOps = expr.split(" ").filter(operators.contains).toList
    def getNums = expr.split(" ").filterNot(operators.contains).map(_.toInt).toList

    def simpleOp(op1: Int, op2: Int, op: String) =
      op match
        case "+" => op1 + op2
        case "-" => op1 - op2
        case "*" => op1 * op2
        case "/" => op1 / op2
        case _ => sys.error("Invalid operator")

    def priority(op: String): Int =
      op match
        case "+" | "-" => 1
        case "*" | "/" => 2
        case _ => 0

    def recurse(
        remainingOperands: List[Int],
        remainingOperators: List[String],
        operandStack: List[Int] = List.empty,
        operatorStack: List[String] = List.empty
    ): Int =
      if remainingOperands.isEmpty then
        if operatorStack.isEmpty then operandStack.head
        else
          val op2 = operandStack.head
          val op1 = operandStack.tail.head
          val op = operatorStack.head
          val res = simpleOp(op1, op2, op)

          recurse(
            remainingOperands,
            remainingOperators,
            res :: operandStack.drop(2),
            operatorStack.tail
          )
      else if remainingOperands.length > remainingOperators.length then
        recurse(
          remainingOperands.tail,
          remainingOperators,
          remainingOperands.head :: operandStack,
          operatorStack
        )
      else if operatorStack.isEmpty ||
        priority(operatorStack.head) < priority(remainingOperators.head)
      then
        recurse(
          remainingOperands,
          remainingOperators.tail,
          operandStack,
          remainingOperators.head :: operatorStack
        )
      else
        val op2 = operandStack.head
        val op1 = operandStack.tail.head
        val op = operatorStack.head
        val res = simpleOp(op1, op2, op)

        recurse(
          remainingOperands,
          remainingOperators,
          res :: operandStack.drop(2),
          operatorStack.tail
        )

    recurse(getNums, getOps)

  println(eval("1 * 3 + 8 / 2"))

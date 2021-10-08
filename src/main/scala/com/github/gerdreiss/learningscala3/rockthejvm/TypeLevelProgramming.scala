package com.github.gerdreiss
package learningscala3
package rockthejvm

object TypeLevelProgramming:

  // Peano arithmetic
  trait Nat
  class _0             extends Nat // 0
  class Succ[N <: Nat] extends Nat // consecutive numbers

  // 2 = succ(succ(0))
  type _1  = Succ[_0]
  type _2  = Succ[_1]
  type _3  = Succ[_2]
  type _4  = Succ[_3]
  type _5  = Succ[_4]
  type _6  = Succ[_5]
  type _7  = Succ[_6]
  type _8  = Succ[_7]
  type _9  = Succ[_8]
  type _10 = Succ[_9]

  /*
   * Axioms:
   *
   * 1st :: 0 < Succ of any other number
   * 2nd :: A < B <=> Succ(A) < Succ(B)
   *
   * 3 < 5
   * 2 < 4
   * 1 < 3
   * 0 < 2 <=> 0 < Succ(1) => true
   */

  // less than
  trait <[A <: Nat, B <: Nat]
  object < :
    given basic[N <: Nat]: <[_0, Succ[N]] with                                   {}
    given inductive[A <: Nat, B <: Nat](using <[A, B]): <[Succ[A], Succ[B]] with {}

    def apply[A <: Nat, B <: Nat](using lt: <[A, B]): <[A, B] = lt // summon resp. implicitly

  //val threeLtOne = <[_3, _0] // does not compile
  val `0 < 2`  = <[_0, _2]  // 0 < 2
  val `1 < 3`  = <[_1, _3]  // 1 < 3
  val `9 < 10` = <[_9, _10] // 9 < 10

  /*
   * <.apply requires <[_1, _3]
   * inductive can create <[_1, _3] if it has access to <[_0, _2]
   * can use basic[_1] to create <[_0, Succ[_1]] = <[_0, _2]
   */

  /*
   * Axioms:
   *
   * 1) 0 <= any other number
   * 2) A <= B iff Succ[A] <= Succ[B]
   *
   */
  trait <=[A <: Nat, B <: Nat]
  object <= :
    given basic[N <: Nat]: <=[_0, N] with                                          {}
    given inductive[A <: Nat, B <: Nat](using <=[A, B]): <=[Succ[A], Succ[B]] with {}

    def apply[A <: Nat, B <: Nat](using lte: <=[A, B]): <=[A, B] = lte // summon resp. implicitly

  // val invalid  = <=[4, 1] // does not compile
  val `0 <= 0` = <=[_0, _0]
  val `1 <= 1` = <=[_1, _1]
  val `3 <= 5` = <=[_3, _5]

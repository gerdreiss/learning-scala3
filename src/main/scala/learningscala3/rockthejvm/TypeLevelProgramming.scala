package com.github.gerdreiss
package learningscala3
package rockthejvm

object TypeLevelProgramming extends App:

  // Peano arithmetic
  trait Nat
  class _0 extends Nat // 0
  class Succ[N <: Nat] extends Nat // consecutive numbers

  // 2 = succ(succ(0))
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
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
    given basic[N <: Nat]: <[_0, Succ[N]] with {}
    given inductive[A <: Nat, B <: Nat](using <[A, B]): <[Succ[A], Succ[B]] with {}

    def apply[A <: Nat, B <: Nat](using lt: <[A, B]): <[A, B] = lt // summon resp. implicitly

  //val threeLtOne = <[_3, _0] // does not compile
  val `0 < 2` = <[_0, _2] // 0 < 2
  val `1 < 3` = <[_1, _3] // 1 < 3
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
    given basic[N <: Nat]: <=[_0, N] with {}
    given inductive[A <: Nat, B <: Nat](using <=[A, B]): <=[Succ[A], Succ[B]] with {}

    def apply[A <: Nat, B <: Nat](using lte: <=[A, B]): <=[A, B] = lte // summon resp. implicitly

  // val invalid  = <=[4, 1] // does not compile
  val `0 <= 0` = <=[_0, _0]
  val `1 <= 1` = <=[_1, _1]
  val `3 <= 5` = <=[_3, _5]

  // list
  trait HList
  class HNil extends HList
  infix class ::[H <: Nat, T <: HList] extends HList

  /*
   * Quicksort:
   * - take the head of the list => pivot
   * - partition list into [elements <= pivot] [elements > pivot]
   * - soft the partitions recursevly
   * - concat the sorted partitions
   *
   * Example [2,3,1,4]
   * - partitioning [1] [3,4]
   * - sort [1] -> [1]
   * - sort [3,4] -> [3,4]
   * - combine the sorted partitions: [1] + 2 + [3,4] = [1,2,3,4]
   */

  // op 1 - concat
  /*
   * - HNil ++ L == L, vor every L <: HList
   * - HA ++ HB        =>       (N :: HA) ++ HB == N :: O
   */
  trait Concat[HA <: HList, HB <: HList, O <: HList] // HA ++ HB == O
  object Concat:
    given basic[L <: HList]: Concat[HNil, L, L] with {}
    given inductive[N <: Nat, HA <: HList, HB <: HList, O <: HList](using
        Concat[HA, HB, O]
    ): Concat[N :: HA, HB, N :: O] with {}

    // summon
    def apply[HA <: HList, HB <: HList, O <: HList](using concat: Concat[HA, HB, O]) = concat

    // this does not compile since [0,1] ++ [3,3] != [1,1,2,3]
    //val invalid = Concat[_0 :: _1 :: HNil, _2 :: _3 :: HNil, _1 :: _1 :: _2 :: _3 :: HNil]
    val concat = Concat[_0 :: _1 :: HNil, _2 :: _3 :: HNil, _0 :: _1 :: _2 :: _3 :: HNil]

  // op 2 - partition
  /*
   * - [] -> ([],[])
   * - [n] -> ([n], [])
   * - P :: T -> (P :: L, R) then for every N <: Nat
   *   - if N <= P then P :: N :: T -> (P :: N :: L, R)
   *   - if N > P  then P :: N :: T -> (P :: L, N :: R)
   */
  trait Partition[HL <: HList, L <: HList, R <: HList]
  object Partition:
    given basicEmpty: Partition[HNil, HNil, HNil] with {}
    given basicOne[N <: Nat]: Partition[N :: HNil, N :: HNil, HNil] with {}

    given inductiveLTE[P <: Nat, N <: Nat, T <: HList, L <: HList, R <: HList](using
        Partition[P :: T, P :: L, R],
        <=[N, P]
    ): Partition[P :: N :: T, P :: N :: L, R] with {}

    given inductiveGT[P <: Nat, N <: Nat, T <: HList, L <: HList, R <: HList](using
        Partition[P :: T, P :: L, R],
        <[P, N]
    ): Partition[P :: N :: T, P :: L, N :: R] with {}

    def apply[HL <: HList, L <: HList, R <: HList](using partition: Partition[HL, L, R]): Partition[HL, L, R] =
      partition

  /*
   * - apply requires a Partition[_2 :: _3 :: _1 :: _4 :: HNil, _2 :: _1 :: HNil, _3 :: _4 :: HNil]
   * - inductiveGT requires a Partition[_2 :: _1 :: _4 :: HNil, _2 :: _1 :: HNil, _4 :: HNil]
   * - inductiveLTE requires a Partition[_2 :: _4 :: HNil, _2 :: HNil, _4 :: HNil]
   * - inductiveGT requires a Partition[_2 :: HNil, _2 :: HNil, HNil]
   * - basicOne generates a Partition[_2 :: HNil, _2 :: HNil, HNil]
   */
  val partition = Partition[_2 :: _3 :: _1 :: _4 :: HNil, _2 :: _1 :: HNil, _3 :: _4 :: HNil]

  // op 3 - quicksort
  /*
   * - sorting the empty list = empty list
   * - for a list N :: T, if we have
   *   - Partition[N :: T, N :: L, R]
   *   - QSort[L, SL]
   *   - QSort[R, SR]
   *   - Concat[SL, N :: R, O]
   * then QSort[N :: T, O]
   */
  trait QSort[L <: HList, O <: HList]
  object QSort:
    given basic: QSort[HNil, HNil] with {}
    given inductive[N <: Nat, T <: HList, L <: HList, R <: HList, SL <: HList, SR <: HList, O <: HList](using
        Partition[N :: T, N :: L, R],
        QSort[L, SL],
        QSort[R, SR],
        Concat[SL, N :: R, O]
    ): QSort[N :: T, O] with {}

    def apply[L <: HList, O <: HList](using sort: QSort[L, O]): QSort[L, O] = sort

  // does not compile
  // val qsortTest2 = QSort[_3 :: _2 :: _1 :: _4 :: HNil, _1 :: _2 :: _4 :: _3 :: HNil]
  val qsortTest = QSort[_3 :: _2 :: _1 :: _4 :: HNil, _1 :: _2 :: _3 :: _4 :: HNil]

  // an implementation of the types without having to know the result (see QSort above)
  trait Sort[L <: HList]:
    type Result <: HList

  object Sort:
    type QSort[L <: HList, O <: HList] = Sort[L] { type Result = O }

    given basic: QSort[HNil, HNil] = new Sort[HNil] { type Result = HNil }
    given inductive[N <: Nat, T <: HList, L <: HList, R <: HList, SL <: HList, SR <: HList, O <: HList](using
        Partition[N :: T, N :: L, R],
        QSort[L, SL],
        QSort[R, SR],
        Concat[SL, N :: R, O]
    ): QSort[N :: T, O] = new Sort[N :: T] { type Result = O }

    def apply[L <: HList](using sort: Sort[L]): QSort[L, sort.Result] = sort

  val sortTest = Sort[_4 :: _3 :: _2 :: _1 :: HNil]

  import org.tpolecat.typename.TypeName

  def printType[A](value: A)(using typename: TypeName[A]): String = typename.value

  // this results in:
  // .::[.Succ[._0], .::[.Succ[._1], .::[.Succ[._2], .::[.Succ[._3], .HNil]]]]
  // which is the same as _1 :: _2 :: _3 :: _4 in term of their successors
  println(
    printType(sortTest)
      .replaceAll("com.github.gerdreiss.learningscala3.rockthejvm.TypeLevelProgramming", "")
  )

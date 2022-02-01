package lists

import java.util as ju
import scala.annotation.tailrec

sealed abstract class RList[+T]: // our covariant list

  def apply(index: Int): T
  def isEmpty: Boolean
  def headOption: Option[T]
  def head: T
  def tail: RList[T]
  def length: Int
  def reverse: RList[T]
  def :+[S >: T](elem: S): RList[S]
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def ++[S >: T](that: RList[S]): RList[S]

end RList

case object RNil extends RList[Nothing]:

  override def apply(index: Int): Nothing  = throw ju.NoSuchElementException()
  override def isEmpty: Boolean            = true
  override def headOption: Option[Nothing] = None
  override def head: Nothing               = throw ju.NoSuchElementException()
  override def tail: RList[Nothing]        = throw ju.NoSuchElementException()
  override def length: Int                 = 0
  def :+[S >: Nothing](elem: S): RList[S]  = elem :: this
  override def reverse: RList[Nothing]     = this
  def ++[S >: Nothing](that: RList[S])     = that

  override def toString: String = "[]"

end RNil

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T]:
  override def apply(index: Int): T =
    if index == 0 then head
    else if index < 0 || tail.isEmpty then throw java.lang.IndexOutOfBoundsException()
    else tail(index - 1)

  override def isEmpty: Boolean = false

  override def headOption: Option[T] = Some(head)

  override def length: Int =
    // stack overflow danger here:
    // 1 + tail.length
    // therefore a tail recursive function:
    @tailrec
    def lengthTailrec(remaining: RList[T], acc: Int): Int =
      if remaining.isEmpty then acc
      else lengthTailrec(remaining.tail, acc + 1)

    lengthTailrec(this, 0)

  override def :+[S >: T](elem: S): RList[S] =
    if tail.isEmpty then head :: elem :: tail
    else head :: (tail :+ elem)

  override def ++[S >: T](that: RList[S]): RList[S] =
    @tailrec
    def concatTailrec(remaining: RList[S], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc
      else concatTailrec(remaining.tail, remaining.head :: acc)

    concatTailrec(that, reverse).reverse

  override def reverse: RList[T] =
    // this doesn't work - obviously, if you think about it...
    @tailrec
    def reverseTailrecG(remaining: RList[T], acc: RList[T]): RList[T] =
      if remaining.isEmpty then acc
      else reverseTailrecG(remaining.tail, acc :+ remaining.head)

    @tailrec
    def reverseTailrecDan(remaining: RList[T], acc: RList[T]): RList[T] =
      if remaining.isEmpty then acc
      else reverseTailrecDan(remaining.tail, remaining.head :: acc)

    reverseTailrecDan(this, RNil)

  override def toString: String =
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String =
      // Dan's code
      //   if remaining.isEmpty then result
      //   else if remaining.tail.isEmpty then s"$result${remaining.head}"
      //   else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
      // my code
      remaining match
        case RNil        => result
        case ::(h, RNil) => s"$result$h"
        case ::(h, t)    => toStringTailrec(t, s"$result$h, ")

    s"[${toStringTailrec(this, "")}]"

end ::

object RList:
  def from[T](iterable: Iterable[T]): RList[T] =
    @tailrec
    def fromTailrec(remaining: Iterable[T], acc: RList[T]): RList[T] =
      if remaining.isEmpty then acc
      else fromTailrec(remaining.drop(1), remaining.head :: acc)

    fromTailrec(iterable, RNil).reverse

object ListProblems extends App:

  val largeList = RList.from((1 to 10000))

  println(RNil)
  println(::(1, ::(2, ::(3, RNil))))
  println("a" :: "b" :: "c" :: RNil)

  println(largeList(0))
  println(largeList(2))
  // println(l(3))  // IndexOutOfBoundsException
  // println(l(-1)) // IndexOutOfBoundsException
  // println(RNil(0)) // NoSuchElementException
  println(largeList.length)
  println(RNil.length)
  println(largeList.reverse)
  println(largeList.reverse :+ "d")
  println(largeList ++ (10001 :: 10002 :: 10003 :: 10004 :: 10005 :: 10006 :: RNil))

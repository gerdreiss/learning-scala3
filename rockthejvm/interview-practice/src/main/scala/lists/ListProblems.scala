package lists

import java.util as ju
import scala.annotation.tailrec

sealed abstract class RList[+T]: // our covariant list
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def headOption: Option[T]
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

case object RNil extends RList[Nothing]:
  override def head: Nothing               = throw ju.NoSuchElementException()
  override def tail: RList[Nothing]        = throw ju.NoSuchElementException()
  override def isEmpty: Boolean            = true
  override def headOption: Option[Nothing] = None

  override def toString: String = "[]"

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T]:
  override def isEmpty: Boolean      = false
  override def headOption: Option[T] = Some(head)

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

object ListProblems extends App:
  println(RNil)
  println(::(1, ::(2, ::(3, RNil))))
  println("a" :: "b" :: "c" :: RNil)

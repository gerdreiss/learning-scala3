package lists

import org.graalvm.compiler.core.common.`type`.ArithmeticOpTable.BinaryOp.Rem

import java.util as ju
import scala.annotation.tailrec
import scala.jdk.FunctionWrappers.RichToLongFunctionAsFunction1
import scala.util.Random

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
  def -[S >: T](elem: S): RList[S]
  def removeAt(index: Int): RList[T]
  def splitAt(index: Int): (RList[T], RList[T])
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(p: T => Boolean): RList[T]
  def rle: RList[(T, Int)]
  def duplicateEach(n: Int): RList[T]
  def rotate(n: Int): RList[T] // rotate by a number of positions to the left
  def sample(n: Int): RList[T]
  def insertionSort[S >: T](ord: Ordering[S]): RList[S]
  def mergeSort[S >: T](ord: Ordering[S]): RList[S]
  def quickSort[S >: T](ord: Ordering[S]): RList[S]

end RList

case object RNil extends RList[Nothing]:

  override def apply(index: Int): Nothing                              = throw ju.NoSuchElementException()
  override def isEmpty: Boolean                                        = true
  override def headOption: Option[Nothing]                             = None
  override def head: Nothing                                           = throw ju.NoSuchElementException()
  override def tail: Nothing                                           = throw ju.NoSuchElementException()
  override def length: Int                                             = 0
  override def :+[S >: Nothing](elem: S): RList[S]                     = elem :: this
  override def reverse: RList[Nothing]                                 = this
  override def ++[S >: Nothing](that: RList[S]): RList[S]              = that
  override def -[S >: Nothing](elem: S): RList[Nothing]                = this
  override def removeAt(index: Int): Nothing                           = throw ju.NoSuchElementException()
  override def splitAt(index: Int): Nothing                            = throw ju.NoSuchElementException()
  override def map[S](f: Nothing => S): RList[S]                       = this
  override def flatMap[S](f: Nothing => RList[S]): RList[S]            = this
  override def filter(p: Nothing => Boolean): RList[Nothing]           = this
  override def rle: RList[(Nothing, Int)]                              = RList.empty[(Nothing, Int)]
  override def duplicateEach(n: Int): RList[Nothing]                   = this
  override def rotate(n: Int): RList[Nothing]                          = this
  override def sample(n: Int): RList[Nothing]                          = this
  override def insertionSort[S >: Nothing](ord: Ordering[S]): RList[S] = this
  override def mergeSort[S >: Nothing](ord: Ordering[S]): RList[S]     = this
  override def quickSort[S >: Nothing](ord: Ordering[S]): RList[S]     = this

  override def toString: String = "[]"

end RNil

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T]:
  override def apply(index: Int): T =
    if index == 0 then head
    else if index < 0 || tail.isEmpty then throw IndexOutOfBoundsException()
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
    // Stack overflow danger here:
    //   if tail.isEmpty then head :: elem :: tail
    //   else head :: (tail :+ elem)
    // this should be better: concat with a list with one element
    ++(elem :: RNil)

  override def ++[S >: T](that: RList[S]): RList[S] =
    @tailrec
    def concatTailrec(remaining: RList[S], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc.reverse
      else concatTailrec(remaining.tail, remaining.head :: acc)

    concatTailrec(that, reverse)

  override def -[S >: T](elem: S): RList[S] =
    @tailrec
    def removeTailrec(remaining: RList[S], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc.reverse
      else if remaining.head == elem then removeTailrec(remaining.tail, acc)
      else removeTailrec(remaining.tail, remaining.head :: acc)

    removeTailrec(this, RList.empty)

  override def removeAt(index: Int): RList[T] =
    @tailrec
    def removeAtTailrec(remaining: RList[T], currentIndex: Int, predecessors: RList[T]): RList[T] =
      if currentIndex == index then predecessors.reverse ++ remaining.tail
      else if remaining.isEmpty then predecessors.reverse
      else removeAtTailrec(remaining.tail, currentIndex + 1, remaining.head :: predecessors)

    if index < 0 then this
    else removeAtTailrec(this, 0, RList.empty)

  override def splitAt(index: Int): (RList[T], RList[T]) =
    @tailrec
    def splitAtTailrec(left: RList[T], right: RList[T], count: Int): (RList[T], RList[T]) =
      if count == 0 then (left.reverse, right)
      else splitAtTailrec(right.head :: left, right.tail, count - 1)

    if index < 0 || index >= length then throw ArrayIndexOutOfBoundsException(index)
    else splitAtTailrec(RList.empty, this, index)

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

    reverseTailrecDan(this, RList.empty)

  override def map[S](f: T => S): RList[S] =
    @tailrec
    def mapTailrec(remaining: RList[T], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc.reverse
      else mapTailrec(remaining.tail, f(remaining.head) :: acc)

    mapTailrec(this, RList.empty)

  override def flatMap[S](f: T => RList[S]): RList[S] =
    @tailrec
    def flatMapTailrecG(remaining: RList[T], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc
      else flatMapTailrecG(remaining.tail, acc ++ f(remaining.head))

    @tailrec
    def flatMapTailrecDan(remaining: RList[T], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc.reverse
      else flatMapTailrecDan(remaining.tail, f(remaining.head).reverse ++ acc)

    // Complexity: O(N + Z)
    @tailrec
    def betterFlatMap(remaining: RList[T], acc: RList[RList[S]]): RList[S] =
      if remaining.isEmpty then RList.concatenateAll(acc)
      else betterFlatMap(remaining.tail, f(remaining.head).reverse :: acc)

    betterFlatMap(this, RList.empty)

  override def filter(p: T => Boolean): RList[T] =
    @tailrec
    def filterTailrec(remaining: RList[T], acc: RList[T]): RList[T] =
      if remaining.isEmpty then acc.reverse
      else if p(remaining.head) then filterTailrec(remaining.tail, remaining.head :: acc)
      else filterTailrec(remaining.tail, acc)

    filterTailrec(this, RList.empty)

  override def rle: RList[(T, Int)] =
    @tailrec
    def rleTailrecG(remaining: RList[T], acc: RList[(T, Int)]): RList[(T, Int)] =
      if remaining.isEmpty then acc.reverse
      else if acc.isEmpty || remaining.head != acc.head._1 then
        val newHead = (remaining.head, 1)
        rleTailrecG(remaining.tail, newHead :: acc)
      else
        val newHead = (remaining.head, acc.head._2 + 1)
        rleTailrecG(remaining.tail, newHead :: acc.tail)

    // Complexity: O(N)
    @tailrec
    def rleTailrecDan(
        remaining: RList[T],
        currentTuple: (T, Int),
        acc: RList[(T, Int)]
    ): RList[(T, Int)] =
      if remaining.isEmpty && currentTuple._2 == 0 then acc
      else if remaining.isEmpty then currentTuple :: acc
      else if remaining.head == currentTuple._1 then
        rleTailrecDan(remaining.tail, currentTuple.copy(_2 = currentTuple._2 + 1), acc)
      else rleTailrecDan(remaining.tail, (remaining.head, 1), currentTuple :: acc)

    // rleTailrecDan(tail, (head, 1), RList.empty).reverse
    rleTailrecG(this, RList.empty)

  override def duplicateEach(n: Int): RList[T] =
    @tailrec
    def duplicateEachTailrecG(remaining: RList[T], acc: RList[T]): RList[T] =
      if remaining.isEmpty then acc
      else
        duplicateEachTailrecG(
          remaining.tail,
          acc ++ RList.from(Iterable.fill(n)(remaining.head))
        )

    // Complexity: O(N * K)
    @tailrec
    def duplicateEachTailrecDan(
        remaining: RList[T],
        currentElement: T,
        nDups: Int,
        acc: RList[T]
    ): RList[T] =
      if remaining.isEmpty && nDups == n then acc
      else if remaining.isEmpty then
        duplicateEachTailrecDan(remaining, currentElement, nDups + 1, currentElement :: acc)
      else if nDups == n then duplicateEachTailrecDan(remaining.tail, remaining.head, 0, acc)
      else duplicateEachTailrecDan(remaining, currentElement, nDups + 1, currentElement :: acc)

    duplicateEachTailrecG(this, RList.empty)

  override def rotate(n: Int): RList[T] =
    @tailrec
    def rotateTailrecG(remaining: RList[T], currentShift: Int, acc: RList[T]): RList[T] =
      if currentShift == n % length then remaining ++ acc.reverse
      else rotateTailrecG(remaining.tail, currentShift + 1, remaining.head :: acc)

    @tailrec
    def rotateTailrecGDan(remaining: RList[T], rotationsLeft: Int, buffer: RList[T]): RList[T] =
      if remaining.isEmpty && rotationsLeft == 0 then this
      else if remaining.isEmpty then rotateTailrecGDan(this, rotationsLeft, RList.empty)
      else if rotationsLeft == 0 then remaining ++ buffer.reverse
      else rotateTailrecGDan(remaining.tail, rotationsLeft - 1, remaining.head :: buffer)

    rotateTailrecG(this, 0, RList.empty)

  override def sample(n: Int): RList[T] =
    @tailrec
    def sampleTailrec(remaining: RList[T], nLeft: Int, acc: RList[T]): RList[T] =
      if nLeft == 0 || remaining.isEmpty then acc
      else
        val randomIndex = Random.nextInt(remaining.length)
        sampleTailrec(remaining.removeAt(randomIndex), nLeft - 1, remaining(randomIndex) :: acc)

    // Dan's solution
    // This solution could result in repeated numbers should the random index repeat
    // Complexity: O(N * K)
    @tailrec
    def sampleTailrecDan(nRemaining: Int, acc: RList[T]): RList[T] =
      if nRemaining == 0 then acc
      else
        val index     = Random.nextInt(length)
        val newNumber = apply(index)
        sampleTailrecDan(nRemaining - 1, newNumber :: acc)

    // Complexity: O(N * K)
    def sampleElegant: RList[T] =
      RList
        .from(1 to n)
        .map(_ => Random.nextInt(length))
        .map(index => apply(index))

    if n < 0 then RList.empty
    else sampleTailrec(this, n, RList.empty)

  override def insertionSort[S >: T](ord: Ordering[S]): RList[S] =
    // Complexity: O(N)
    @tailrec
    def insertSorted(elem: T, before: RList[S], after: RList[S]): RList[S] =
      if after.isEmpty || ord.lteq(elem, after.head) then before.reverse ++ (elem :: after)
      else insertSorted(elem, after.head :: before, after.tail)
    // Complexity: O(N^2)
    @tailrec
    def insertSortTailrec(remaining: RList[T], acc: RList[S]): RList[S]    =
      if remaining.isEmpty then acc
      else insertSortTailrec(remaining.tail, insertSorted(remaining.head, RList.empty, acc))

    insertSortTailrec(this, RList.empty)

  override def mergeSort[S >: T](ord: Ordering[S]): RList[S] =
    // Dan's solution
    @tailrec
    def mergeDan(a: RList[S], b: RList[S], acc: RList[S]): RList[S] =
      if a.isEmpty then acc.reverse ++ b
      else if b.isEmpty then acc.reverse ++ a
      else if ord.lteq(a.head, b.head) then mergeDan(a.tail, b, a.head :: acc)
      else mergeDan(a, b.tail, b.head :: acc)

    // Complexity: O(n * log(n))
    @tailrec
    def mergeSortDan(smallLists: RList[RList[S]], bigLists: RList[RList[S]]): RList[S] =
      if smallLists.isEmpty then
        if bigLists.isEmpty then RList.empty
        else if bigLists.tail.isEmpty then bigLists.head
        else mergeSortDan(bigLists, RList.empty)
      else if smallLists.tail.isEmpty then
        if bigLists.isEmpty then smallLists.head
        else mergeSortDan(smallLists.head :: bigLists, RList.empty)
      else
        val first  = smallLists.head
        val second = smallLists.tail.head
        val merged = mergeDan(first, second, RList.empty)
        mergeSortDan(smallLists.tail.tail, merged :: bigLists)

    // Solution inspired by https://medium.com/analytics-vidhya/playing-with-scala-merge-sort-d382fb1a32ff
    @tailrec
    def mergeG(left: RList[S], right: RList[S], acc: RList[S] = RList.empty): RList[S] =
      (left, right) match
        case (RNil, _)          => acc ++ right
        case (_, RNil)          => acc ++ left
        case (l :: ls, r :: rs) =>
          if ord.lteq(l, r) then mergeG(ls, right, acc :+ l)
          else mergeG(left, rs, acc :+ r)

    def mergeSortG(list: RList[S]): RList[S] =
      if list.length < 2 then list
      else
        val (left, right) = list.splitAt(list.length / 2)
        mergeG(mergeSortG(left), mergeSortG(right))

    // mergeSortDan(map(_ :: RNil), RNil)
    mergeSortG(this)

  override def quickSort[S >: T](ord: Ordering[S]): RList[S] =
    @tailrec
    def partition(
        list: RList[T],
        pivot: T,
        smaller: RList[T],
        larger: RList[T]
    ): (RList[T], RList[T]) =
      if list.isEmpty then (smaller, larger)
      else if ord.lteq(list.head, pivot) then
        partition(list.tail, pivot, list.head :: smaller, larger)
      else partition(list.tail, pivot, smaller, list.head :: larger)

    // Complexity: O(N^2) in the worst case (when the list is sorted)
    // on average O(N 8 log(N))
    @tailrec
    def quickSortTailrec(remainingLists: RList[RList[T]], acc: RList[RList[T]]): RList[T] =
      if remainingLists.isEmpty then acc.flatMap(identity).reverse
      else if remainingLists.head.isEmpty then quickSortTailrec(remainingLists.tail, acc)
      else if remainingLists.head.tail.isEmpty then
        quickSortTailrec(remainingLists.tail, remainingLists.head :: acc)
      else
        val remainingList     = remainingLists.head
        val pivot             = remainingList.head
        val listToSplit       = remainingList.tail
        val (smaller, larger) = partition(listToSplit, pivot, RList.empty, RList.empty)
        quickSortTailrec(smaller :: (pivot :: RNil) :: larger :: remainingLists.tail, acc)

    quickSortTailrec(this :: RNil, RList.empty)

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
  def empty[T]: RList[T] = RNil

  def from[T](iterable: Iterable[T]): RList[T] =
    @tailrec
    def fromTailrec(remaining: Iterable[T], acc: RList[T]): RList[T] =
      if remaining.isEmpty then acc.reverse
      else fromTailrec(remaining.drop(1), remaining.head :: acc)

    fromTailrec(iterable, RList.empty)

  def concatenateAll[T](lists: RList[RList[T]]): RList[T] =
    @tailrec
    def concatenateAll[S](elements: RList[RList[S]], current: RList[S], acc: RList[S]): RList[S] =
      if current.isEmpty && elements.isEmpty then acc
      else if current.isEmpty then concatenateAll(elements.tail, elements.head, acc)
      else concatenateAll(elements, current.tail, current.head :: acc)

    concatenateAll(lists, empty, empty)

end RList

object ListProblems extends App:
  val len = 10000

  val smallList = 1 :: 2 :: 3 :: 4 :: 5 :: RNil
  val largeList = RList.from(1 to len)

  // println(RList.empty)
  // println(smallList)
  // println("a" :: "b" :: "c" :: RNil)
  // println(largeList(0))
  // println(largeList(2))
  // println(l(3))  // IndexOutOfBoundsException
  // println(l(-1)) // IndexOutOfBoundsException
  // println(RNil(0)) // NoSuchElementException
  // println(largeList.length)
  // println(RNil.length)
  // println(largeList.reverse)
  // println(largeList.reverse :+ "d")
  // println(largeList ++ (len + 1 :: len + 2 :: len + 3 :: len + 4 :: len + 5 :: RNil))
  // println(largeList - (len - 1))
  // println(largeList.removeAt(len - 1))
  // println(smallList.map(_ * 10))
  // println(smallList.flatMap(x => x :: 2 * x :: RNil))
  // println(smallList.filter(_ % 2 == 0))
  // println(largeList.flatMap(x => x :: x * 2 :: RNil))

  // val start      = System.currentTimeMillis
  // val flatMapped = largeList.flatMap(x => x :: x * 2 :: x * 3 :: x * 4 :: RNil)
  // val lapse      = System.currentTimeMillis - start
  // println(flatMapped)
  // println(lapse)

  println("-" * 100)
  val rleList = 1 :: 1 :: 2 :: 3 :: 3 :: 5 :: 6 :: 6 :: 6 :: 6 :: RNil
  println(rleList)
  println(rleList.rle)
  println("-" * 100)
  println(smallList.duplicateEach(3))
  println("-" * 100)
  println(smallList.rotate(113))
  println("-" * 100)
  println(smallList.sample(20))
  println(largeList.sample(50))
  println("-" * 100)

  private val value: Ordering[Int] = Ordering.fromLessThan(_ < _)

  val unsortedList    = 10 :: 3 :: 30 :: 100 :: 3 :: 1 :: 2 :: RNil
  val anotherUnsorted = largeList.sample(20)

  println(s"Unsorted       : $anotherUnsorted")
  println(s"Insertion sort : ${anotherUnsorted.insertionSort(value)}")
  println(s"Merge sort     : ${anotherUnsorted.mergeSort(value)}")
  println(s"Quick sort     : ${anotherUnsorted.quickSort(value)}")

package exercises

trait MySet[A] extends (A => Boolean):
  def apply(a: A): Boolean = contains(a)

  def contains(a: A): Boolean
  def +(a: A): MySet[A]
  def ++(that: MySet[A]): MySet[A]
  def -(a: A): MySet[A]
  def --(that: MySet[A]): MySet[A]
  def &(that: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(p: A => Boolean): MySet[A]
  def filterNot(p: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
  def unary_! : MySet[A]
end MySet

class EmptySet[A]() extends MySet[A]:
  override def contains(a: A): Boolean = false
  override def +(a: A): MySet[A] = NonEmptySet[A](a, this)
  override def ++(that: MySet[A]): MySet[A] = that
  override def -(a: A): MySet[A] = this
  override def --(that: MySet[A]): MySet[A] = this
  override def &(that: MySet[A]): MySet[A] = this
  override def map[B](f: A => B): MySet[B] = EmptySet[B]()
  override def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet[B]()
  override def filter(p: A => Boolean): MySet[A] = this
  override def filterNot(p: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()
  override def unary_! : MySet[A] = PropertyBasedSet(_ => true)
end EmptySet

// this one is useless
class AllInclusiveSet[A] extends MySet[A]:
  def contains(a: A): Boolean = true
  def +(a: A): MySet[A] = this
  def ++(that: MySet[A]): MySet[A] = this
  def -(a: A): MySet[A] = this // impossible to implement
  def --(that: MySet[A]): MySet[A] = filter(!that)
  def &(that: MySet[A]): MySet[A] = filter(that)
  def map[B](f: A => B): MySet[B] = ??? // impossible to implement
  def flatMap[B](f: A => MySet[B]): MySet[B] = ??? // impossible to implement
  def filter(p: A => Boolean): MySet[A] = ??? // property-based set
  def filterNot(p: A => Boolean): MySet[A] = ??? // property-based set?
  def foreach(f: A => Unit): Unit = ??? // impossible to implement
  def unary_! : MySet[A] = EmptySet()
end AllInclusiveSet

class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]:
  def contains(a: A): Boolean = property(a)
  def +(a: A): MySet[A] = PropertyBasedSet(x => property(x) || x == a)
  def ++(that: MySet[A]): MySet[A] = PropertyBasedSet(x => property(x) || that.contains(x))
  def -(a: A): MySet[A] = filter(_ != a)
  def --(that: MySet[A]): MySet[A] = filter(!that)
  def &(that: MySet[A]): MySet[A] = filter(that)
  def map[B](f: A => B): MySet[B] = politelyFail
  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  def filter(p: A => Boolean): MySet[A] = PropertyBasedSet(x => property(x) && p(x))
  def filterNot(p: A => Boolean): MySet[A] = PropertyBasedSet(x => property(x) && !p(x))
  def foreach(f: A => Unit): Unit = politelyFail
  def unary_! : MySet[A] = PropertyBasedSet(x => !property(x))

  private def politelyFail =
    throw java.lang.IllegalArgumentException("Really deep rabbit hole!")

end PropertyBasedSet

class NonEmptySet[A](val head: A, val tail: MySet[A]) extends MySet[A]:
  override def contains(a: A): Boolean =
    a == head || tail.contains(a)

  override def +(a: A): MySet[A] =
    if contains(a) then this
    else NonEmptySet(a, this)

  override def ++(that: MySet[A]): MySet[A] =
    tail ++ that + head

  override def -(a: A): MySet[A] =
    // Daniel's solution:
    // if head == a then tail
    // else tail - a + head
    // my solution (I like mine better)
    filter(_ != a)

  override def --(that: MySet[A]): MySet[A] =
    // my solution
    // (for
    //   a1 <- this
    //   a2 <- that
    // yield (a1, a2))
    //   .filter(_ != _)
    //   .map(_._1)
    // Daniel's solution (definitely a better one, needs the implementation of `def unary_!` though)
    filter(!that)

  override def &(that: MySet[A]): MySet[A] =
    // my solution
    // (for
    //   a1 <- this
    //   a2 <- that
    // yield (a1, a2))
    //   .filter(_ == _)
    //   .map(_._1)
    // Daniel's solution, very elegant
    filter(that)

  override def map[B](f: A => B): MySet[B] =
    tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] =
    f(head) ++ tail.flatMap(f)

  override def filter(p: A => Boolean): MySet[A] =
    if p(head) then tail.filter(p) + head
    else tail.filter(p)

  override def filterNot(p: A => Boolean): MySet[A] =
    if p(head) then tail.filter(p)
    else tail.filter(p) + head

  override def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)

  override def unary_! : MySet[A] =
    PropertyBasedSet(x => !this.contains(x))

end NonEmptySet

object MySet:
  def apply[A](values: A*): MySet[A] =
    @annotation.tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if valSeq.isEmpty then acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, EmptySet())

@main def testMySet(): Unit =
  val s = MySet(1, 2, 3, 4)
  (s + 5 ++ MySet(-1, -2) + 3)
    .map(_ * 10)
    .flatMap(x => MySet(x, x + 1))
    .filter(_ % 2 == 0)
    .foreach(println)

  val notS = !s // all the naturals not equal to 1,2,3,4
  println(notS(2)) // false
  println(notS(5)) // true

  val evenNotS = notS.filter(_ % 2 == 0)
  println(evenNotS(5)) // false

  val `evenNotS + 5` = evenNotS + 5
  println(`evenNotS + 5`(5)) // true

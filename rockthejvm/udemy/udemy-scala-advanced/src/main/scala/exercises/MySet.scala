package exercises

trait MySet[A] extends (A => Boolean):
  def apply(a: A): Boolean = contains(a)

  def contains(a: A): Boolean
  def +(a: A): MySet[A]
  def ++(that: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(p: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

class EmptySet[A]() extends MySet[A]:
  override def contains(a: A): Boolean = false
  override def +(a: A): MySet[A] = NonEmptySet[A](a, this)
  override def ++(that: MySet[A]): MySet[A] = that
  override def map[B](f: A => B): MySet[B] = EmptySet[B]()
  override def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet[B]()
  override def filter(p: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()

class NonEmptySet[A](val head: A, val tail: MySet[A]) extends MySet[A]:
  override def contains(a: A): Boolean =
    a == head || tail.contains(a)

  override def +(a: A): MySet[A] =
    if contains(a) then this
    else NonEmptySet(a, this)

  override def ++(that: MySet[A]): MySet[A] =
    tail ++ that + head

  override def map[B](f: A => B): MySet[B] =
    tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] =
    f(head) ++ tail.flatMap(f)

  override def filter(p: A => Boolean): MySet[A] =
    if p(head) then tail.filter(p) + head
    else tail.filter(p)

  override def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)

object MySet:
  def apply[A](values: A*): MySet[A] =
    @annotation.tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if valSeq.isEmpty then acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, EmptySet())

@main def testMySet(): Unit =
  val s = MySet(1, 2, 3, 4)
  s + 5 ++ MySet(-1, -2) + 3 map (_ * 10) flatMap (x =>
    MySet(x, x + 1)
  ) filter (_ % 2 == 0) foreach println

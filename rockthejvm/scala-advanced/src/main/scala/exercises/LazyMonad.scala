package exercises

class Lazy[+A](value: => A):
  // call by name
  private lazy val internalValue = value
  def get: A = value
  // for this to be truly lazy, the function must be lazy
  // the function param must be by name
  // to make sure it is by name, put it in parens!
  def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue) // OMG! this is possible?!
  def map[B](f: (=> A) => B): Lazy[B] = flatMap(a => Lazy(f(a)))
  def flatten[B >: A](m: Lazy[Lazy[B]]): Lazy[B] = m.flatMap(identity)

object Lazy:
  def apply[A](value: => A): Lazy[A] = new Lazy(value)

@main def testLazyMonad(): Unit =
  val lazyInstance = Lazy {
    println("is this printed?")
    42
  }
  val lazyAndFlatmapped = lazyInstance.flatMap(x =>
    Lazy {
      println("this should definitely not be printed!")
      x * 10
    }
  )

  println(lazyAndFlatmapped.get)

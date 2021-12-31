object MatchTypes:

  def lastDigitOf(n: BigInt): Int     = (n % 10).toInt
  def lastCharOf(s: String): Char     = s.headOption.getOrElse(throw new NoSuchElementException)
  def lastElementOf[T](l: List[T]): T = l.headOption.getOrElse(throw new NoSuchElementException)

  type ConstituentPartOf[T] =
    T match
      case BigInt  => Int
      case String  => Char
      case List[t] => t

  val digit: ConstituentPartOf[BigInt]  = 2 // ok
  val char: ConstituentPartOf[String]   = 'a'
  val elm: ConstituentPartOf[List[Int]] = 42

  def lastComponentOf[T](biggerValue: T & Matchable): ConstituentPartOf[T] = ???
  // this ain't compiling for some reason...
  // biggerValue match
  //   case b: BigInt  => (b % 10).toInt
  //   case s: String  => s.headOption.getOrElse(throw new NoSuchElementException)
  //   case l: List[T] => l.headOption.getOrElse(throw new NoSuchElementException)

  val lastElm   = lastComponentOf(List(1, 2, 3))     // 3
  val lastChar  = lastComponentOf("Scala")           // 'a'
  val lastDigit = lastComponentOf(BigInt(234234234)) // 4

  type LowestLevelPartOf[T] =
    T match
      case List[t] => LowestLevelPartOf[t]
      case ?       => T

  val lastElementOfNestedList: LowestLevelPartOf[List[List[List[Int]]]] = 2 // ok

  // type AnnoyingMatchType[T] =
  //   T match
  //     case ? => AnnoyingMatchType[T]

  type InfiniteRecursiveType[T] =
    T match
      case Int => InfiniteRecursiveType[T]

  def aNaiveMethod[T]: InfiniteRecursiveType[T] = ???

  // compiler error: Recursion limit exceeded
  // val illegal: Int = aNaiveMethod[Int]

  def accumulate[T](accumulator: T, smallerValue: ConstituentPartOf[T]): T = ???

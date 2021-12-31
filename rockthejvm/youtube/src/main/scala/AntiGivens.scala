

object AntiGivens:

  def processLists[A, B](as: List[A], bs: List[B]): List[(A, B)] =
    for {
      a <- as
      b <- bs
    } yield (a, b)

  // process lists of the same type
  object SameType:
    def processListsSameType[A](as: List[A], bs: List[A]): List[(A, A)] =
      for {
        a <- as
        b <- bs
      } yield (a, b)

    def processListsSameTypeV2[A](as: List[A], bs: List[A]): List[(A, A)] =
      processLists(as, bs)

    // type =:=[A,B]
    def processListsSameTypeV3[A, B](as: List[A], bs: List[B])(using A =:= B): List[(A, B)] =
      processLists(as, bs)

    val v1 = processListsSameType(List(1, 2), List("black", "white")) // still compiles
    val v2 = processListsSameTypeV2(List(1, 2), List("black", "white")) // still compiles
    val v3 = ??? // processListsSameTypeV3(List(1, 2), List("black", "white")) // does not compile

  // process lists of different types
  object DiffeerentTypes:
    import scala.util.NotGiven

    def processListsDifferentTypes[A, B](as: List[A], bs: List[B])(using NotGiven[A =:= B]): List[(A, B)] =
      processLists(as, bs)

    val list1 = processListsDifferentTypes(List(1, 2), List("black", "white")) // ok
    val list2 = ??? // processListsDifferentTypes(List(1, 2), List(4, 5)) // does not compile

  // a and b must be different
  val combinedLists = processLists(List(1, 2), List("black", "white"))
  val notOkLists = processLists(List(1, 2), List(4, 5)) // should be prevented

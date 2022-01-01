package lectures.Section3_Lecture09

object PartialFunctions extends App:

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  println(aPartialFunction(2))
  // println(aPartialFunction(98)) this blows up
  println(aPartialFunction.isDefinedAt(98))

  val liftedToTotal = aPartialFunction.lift

  println(liftedToTotal(2))
  println(liftedToTotal(98)) // this returns None, and does not blow up

  val aChainedPartialFunction = aPartialFunction.orElse[Int, Int] { case 45 =>
    67
  }

  println(aChainedPartialFunction(45))

  val aTotalFunction: Int => Int = { case 1 =>
    99
  }

  val aMappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 100
  }

  println(aMappedList)

  /** partial functions can only have ONE parameter type */
  println("PFFTW!")

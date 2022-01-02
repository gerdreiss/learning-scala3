package exercises

object PartialFunctionsExercise extends App:

  val aManualFussyFunction: PartialFunction[Int, Int] =
    new:
      override def apply(x: Int): Int = x match
        case 1 => 42
        case 2 => 58
        case 5 => 999

      override def isDefinedAt(x: Int): Boolean =
        x == 1 || x == 2 || x == 5

  val chatbot: PartialFunction[String, String] = {
    case "hello" => "Hi, I'm HAL9000"
    case "goodbye" => "Once you start, there is no end"
    case "call mom" => "No phone found"
  }

  io.Source.stdin.getLines().map(chatbot).foreach(println)

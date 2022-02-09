package strings

object ReverseWords extends App:

  extension (s: String)
    def words: Array[String] =
      s.split("\\s+")

  def solutionG(text: String): String =
    text.words.reverse.mkString(" ")

  def solutionDan(text: String): String =
    text.split(" ").filter(!_.isEmpty).reverse.mkString(" ")

  println(solutionG("hello, beautiful          scala  world!"))
  println(solutionDan("hello, beautiful          scala  world!"))

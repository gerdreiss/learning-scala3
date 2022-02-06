package strings

object Anagrams extends App:

  extension (s: String)
    def isAnagramOf(a: String): Boolean =
      s.toLowerCase.sorted == a.toLowerCase.sorted

  def solutionG(s: String, anagrams: List[String]): List[String] =
    anagrams.filter(_.isAnagramOf(s))

  def solutionDan1(s: String, a: String): Boolean =
    CountChars.solutionDan(s) == CountChars.solutionDan(a)

  def solutionDan2(s: String, a: String): Boolean =
    s.sorted == a.sorted

  println("desserts".isAnagramOf("stressed"))
  println("Scala".isAnagramOf("Haskell"))

  println(solutionDan1("desserts", "stressed"))
  println(solutionDan1("Scala", "Haskell"))
  println(solutionDan2("desserts", "stressed"))
  println(solutionDan2("Scala", "Haskell"))

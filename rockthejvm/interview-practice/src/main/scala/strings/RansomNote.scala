package strings

object RansomNote extends App:

  def solutionG(note: String, magazine: String): Boolean =
    note
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .forall((char, count) => magazine.count(_ == char) >= count)

  def solutionDan(note: String, magazine: String): Boolean =
    def buildMap(s: String): Map[Char, Int] =
      s.foldLeft(Map.empty[Char, Int]) { (acc, ch) =>
        acc + (ch -> acc.getOrElse(ch, 0))
      }

    val noteMap     = buildMap(note)
    val magazineMap = buildMap(magazine)

    noteMap.keySet.forall(ch => noteMap.getOrElse(ch, 0) <= magazineMap.getOrElse(ch, 0))

  val note     = "Lorem ipsum dolor sit amet"
  val magazine =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

  println(solutionG(note, magazine))
  println(solutionDan(note, magazine))

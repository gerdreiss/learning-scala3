package strings

object ReorganizeString extends App:

  /**
   * Rearrange string so that no two adjucent characters are identical
   */
  // complexity: O(N^2) time
  // Better complexity: use TreeMap
  def solution(s: String): String =
    def recurse(charCount: Map[Char, Int], currentChar: Char = '\u0000', acc: String = ""): String =
      if charCount.isEmpty then acc
      else
        val newChar      =
          charCount.filter((char, _) => char != currentChar).maxBy((_, count) => count)._1
        val newCharCount =
          if charCount(newChar) == 1 then charCount - newChar
          else charCount + (newChar -> (charCount(newChar) - 1))

        recurse(newCharCount, newChar, acc + newChar)

    val charCount = s.groupBy(identity).view.mapValues(_.length).toMap
    if charCount.values.exists(_ > (s.length + 1) / 2) then ""
    else recurse(charCount)

  println(solution("aaab"))
  println(solution("aaabcd"))

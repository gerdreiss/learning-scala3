package strings

import scala.annotation.tailrec

object Justify extends App:

  extension (s: String)
    def words: List[String] =
      s.split("\\s+").toList

  def solutionG(text: String, width: Int): List[String] =
    @tailrec
    def justifyLine(line: String, spaces: Int): String =
      if spaces == 0 then line
      else if spaces < 0 then justifyLine(line.replaceFirst("  ", " "), spaces + 1)
      else
        val newLine = line.replaceAll(" ", "  ")
        justifyLine(newLine, spaces - (newLine.length - line.length))

    @tailrec
    def rec(
        words: List[String],
        line: String = "",
        lines: List[String] = List.empty
    ): List[String] =
      if words.isEmpty then lines
      else if line.length + words.head.length + 1 <= width then
        rec(words.tail, s"$line ${words.head}", lines)
      else rec(words, "", lines :+ line.trim) // justifyLine(line.trim, width - line.trim.length))

    rec(text.words)

  end solutionG

  def solutionDan(text: String, width: Int): String =
    @tailrec
    def pack(
        words: List[String],
        currentRow: List[String] = List.empty,
        currentLen: Int = 0,
        result: List[List[String]] = List.empty
    ): List[List[String]] =
      if words.isEmpty && currentRow.isEmpty then result
      else if words.isEmpty then result :+ currentRow
      else if currentRow.isEmpty && words.head.length > width then
        val (here, there) = words.head.splitAt(width - 2)
        pack(there :: words.tail, List.empty, 0, result :+ List(here + "-"))
      else if words.head.length + currentLen > width then
        pack(words, List.empty, 0, result :+ currentRow)
      else pack(words.tail, currentRow :+ words.head, currentLen + 1 + words.head.length, result)

    def justify(row: List[String]): String =
      if row.length == 1 then row.head
      else
        val nSpacesAvailable  = width - row.map(_.length).sum
        val nIntervals        = row.length - 1
        val spacesPerInterval = nSpacesAvailable / nIntervals
        val nExtraSpaces      = nSpacesAvailable % nIntervals
        val regularSpace      = " " * spacesPerInterval
        val biggerSpace       = " " * (spacesPerInterval + 1)

        if nExtraSpaces == 0 then row.mkString(regularSpace)
        else
          val nWordsWithBiggerIntervals = nExtraSpaces + 1
          val wordsWithBiggerIntervals  = row.take(nWordsWithBiggerIntervals)
          val firstPart                 = wordsWithBiggerIntervals.mkString(biggerSpace)
          val secondPart                = row.drop(nWordsWithBiggerIntervals).mkString(regularSpace)

          firstPart + regularSpace + secondPart

    // split text into words
    // pack the words into rows
    val words       = text.split(" ").toList
    val unjustified = pack(words)
    val justified   = unjustified.init.map(justify) :+ unjustified.last.mkString(" ")

    justified.mkString("\n")

  end solutionDan

  val text =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

  // println(justifyLine("Lorem ipsum dolor sit amet", 4))
  // solutionDan(text, 15).foreach(println)
  solutionG(text, 15).foreach(println)

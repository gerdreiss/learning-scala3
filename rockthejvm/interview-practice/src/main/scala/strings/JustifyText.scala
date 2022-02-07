package strings

import scala.annotation.tailrec

object JustifyText extends App:

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
      if words.isEmpty then lines :+ line
      else if line.length + words.head.length <= width then
        rec(words.tail, s"$line ${words.head}", lines)
      else rec(words, "", lines :+ justifyLine(line.trim, width - line.trim.length))

    rec(text.words)

  end solutionG

  def solutionDanRewritten(text: String, width: Int): String =
    type Words = List[String]
    type Lines = List[Words]

    def pack(words: Words, currentRow: Words, result: Lines): Lines =
      if words.isEmpty && currentRow.isEmpty then result
      else if currentRow.nonEmpty then result :+ currentRow
      else if currentRow.isEmpty && words.head.length > width then
        pack(
          words.head.drop(width - 1) :: words.tail,
          List.empty,
          result :+ List(words.head.take(width - 1) + "-")
        )
      else if currentRow.length + 1 + words.head.length > width then
        pack(words, List.empty, result :+ currentRow)
      else pack(words.tail, currentRow :+ words.head, result)

    def justifyRow(row: List[String]): String =
      if row.length == 1 then row.head
      else
        val nSpacesAvailable   = width - row.map(_.length).sum
        val nIntervals         = row.length - 1
        val nSpacesPerInterval = nSpacesAvailable / nIntervals
        val nExtraSpaces       = nSpacesAvailable % nIntervals
        val regularSpace       = " " * nSpacesPerInterval
        val biggerSpace        = " " * (nSpacesPerInterval + 1)

        if nExtraSpaces == 0 then row.mkString(regularSpace)
        else
          val nWordsWithBiggerIntervals = nExtraSpaces + 1
          val firstPart                 = row.take(nWordsWithBiggerIntervals).mkString(biggerSpace)
          val secondPart                = row.drop(nWordsWithBiggerIntervals).mkString(regularSpace)

          firstPart + " " + secondPart

    val unjustifiedRows = pack(text.words, List.empty, List.empty)
    val justifiedRows   = unjustifiedRows.init.map(justifyRow) :+ unjustifiedRows.head

    justifiedRows.mkString("\n")

  def solutionDan(text: String, width: Int): String =
    @tailrec
    def pack(
        words: List[String],
        currentRow: List[String],
        currentCharCount: Int,
        result: List[List[String]]
    ): List[List[String]] =
      if words.isEmpty && currentRow.isEmpty then result
      else if words.isEmpty then result :+ currentRow
      else if currentRow.isEmpty && words.head.length > width then
        val (partOnThisRow, partOnNextRow) = words.head.splitAt(width - 2)
        pack(partOnNextRow :: words.tail, List.empty, 0, result :+ List(partOnThisRow + "-"))
      else if words.head.length + currentCharCount > width then
        pack(words, List.empty, 0, result :+ currentRow)
      else
        pack(words.tail, currentRow :+ words.head, currentCharCount + 1 + words.head.length, result)

    def justifyRow(row: List[String]): String =
      if row.length == 1 then row.head
      else
        val nSpacesAvailable   = width - row.map(_.length).sum
        val nIntervals         = row.length - 1
        val nSpacesPerInterval = nSpacesAvailable / nIntervals
        val nExtraSpaces       = nSpacesAvailable % nIntervals
        val regularSpace       = " " * nSpacesPerInterval
        val biggerSpace        = " " * (nSpacesPerInterval + 1)

        if nExtraSpaces == 0 then row.mkString(regularSpace)
        else
          val nWordsWithBiggerIntervals = nExtraSpaces + 1
          val firstPart                 = row.take(nWordsWithBiggerIntervals).mkString(biggerSpace)
          val secondPart                = row.drop(nWordsWithBiggerIntervals).mkString(regularSpace)

          firstPart + " " + secondPart

    val words           = text.split(" ").toList
    val unjustifiedRows = pack(words, List.empty, 0, List.empty)
    val justifiedRows   = unjustifiedRows.map(justifyRow)

    justifiedRows.mkString("\n")

  end solutionDan

  val text =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

  println(solutionDan(text, 50))
  println("=" * 100)
  println(solutionDanRewritten(text, 10))

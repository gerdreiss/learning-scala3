package strings

object CompareVersionNumbers extends App:

  def ext(version: String, numDots: Int, numDotsMax: Int): String =
    if numDots == numDotsMax then version
    else version + (".0" * (numDotsMax - numDots))

  def num(version: String): Int =
    version
      .split("\\.")
      .map(_.toInt)
      .reverse
      .zipWithIndex
      .foldLeft(0.0) { case (acc, (n, idx)) =>
        acc + n * math.pow(10, idx)
      }
      .toInt

  // compare software versions
  def solutionG(version1: String, version2: String): Int =
    val numDotsV1  = version1.count(_ == '.')
    val numDotsV2  = version2.count(_ == '.')
    val numDotsMax = numDotsV1 max numDotsV2
    val v1         = ext(version1, numDotsV1, numDotsMax)
    val v2         = ext(version2, numDotsV2, numDotsMax)

    num(v1) compareTo num(v2)

  def solutionDan(version1: String, version2: String): Int =
    def compare(rev1: List[Int], rev2: List[Int]): Int =
      if rev1.isEmpty && rev2.isEmpty then 0
      else if rev1.isEmpty then if rev2.exists(_ != 0) then -1 else 0
      else if rev2.isEmpty then if rev1.exists(_ != 0) then 1 else 0
      else
        val v1 = rev1.head
        val v2 = rev2.head

        if v1 < v2 then -1
        else if v1 > v2 then 1
        else compare(rev1.tail, rev2.tail)

    val sectionsV1 = version1.split("\\.").toList.map(_.toInt)
    val sectionsV2 = version2.split("\\.").toList.map(_.toInt)

    compare(sectionsV1, sectionsV2)

  println(solutionG("0.9", "1.0.1.4"))
  println(solutionG("0.9", "1"))
  println(solutionG("1", "1.0.0"))
  println(solutionG("2.1", "2.01"))

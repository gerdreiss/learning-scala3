package misc

object SierpinskiTriangles extends App:

  /**
   *  n = 1
   *  -----
   *         * 
   *        * *
   *
   *  n = 2
   *  -----
   *         *         
   *        * *
   *       *   *
   *      * * * *
   *
   *  n = 3
   *  -----
   *          *         
   *         * *
   *        *   *
   *       * * * *
   *      *       *  
   *     * *     * *
   *    *   *   *   *
   *   * * * * * * * * 
   *
   */

  def stackrec(n: Int): List[String] =
    if n == 0 then List("*")
    else
      val triangle = stackrec(n - 1)
      val spaces = " " * (1 << (n - 1)) // 2 ^ (n - 1) spaces
      val top = triangle.map(spaces + _ + spaces)
      val bottom = triangle.map(row => row + " " + row)
      top ++ bottom

  def tailrec(n: Int): List[String] =
    def recurse(currentLevel: Int, currentTriangle: List[String] = List("*")): List[String] =
      if currentLevel >= n then currentTriangle
      else
        val spaces = " " * (1 << currentLevel) // 2 ^ currentLevel spaces
        val top = currentTriangle.map(spaces + _ + spaces)
        val bottom = currentTriangle.map(row => row + " " + row)
        recurse(currentLevel + 1, top ++ bottom)

    recurse(0)

  println(stackrec(4).mkString("\n"))
  println(tailrec(4).mkString("\n"))

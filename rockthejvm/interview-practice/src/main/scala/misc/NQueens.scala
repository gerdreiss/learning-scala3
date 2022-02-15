package misc

def nQueens(n: Int): List[List[Int]] =
  def conflict1(position: Int, queen: Int, index: Int): Boolean =
    queen == position || index + 1 == position - queen || index + 1 == queen - position

  def conflict(position: Int, queens: List[Int]): Boolean =
    queens.zipWithIndex.exists((queen, index) => conflict1(position, queen, index))

  def recurse(
      currentPos: Int,
      currentQueens: List[Int] = List.empty,
      solutions: List[List[Int]] = List.empty
  ): List[List[Int]] =
    if currentPos >= n && currentQueens.isEmpty then solutions
    else if currentPos >= n then recurse(currentQueens.head + 1, currentQueens.tail, solutions)
    else if conflict(currentPos, currentQueens) then
      recurse(currentPos + 1, currentQueens, solutions)
    else if currentQueens.length == n - 1 then
      val newSolution = currentPos :: currentQueens
      recurse(currentPos + 1, currentQueens, newSolution :: solutions)
    else recurse(0, currentPos :: currentQueens, solutions)

  recurse(0)

object NQueens extends App:

  println(nQueens(4))

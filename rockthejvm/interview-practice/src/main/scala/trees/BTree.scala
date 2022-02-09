package trees

import java.util.NoSuchElementException
import scala.annotation.tailrec

sealed trait BTree[+T]:
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean
  def isLeaf: Boolean
  def size: Int
  def collectLeaves: List[BTree[T]]
  def leafCount: Int = collectLeaves.length
  def collectNodes(level: Int): List[BTree[T]]
  def mirror: BTree[T]
end BTree

case object BEnd extends BTree[Nothing]:

  override def value: Nothing = throw NoSuchElementException()

  override def left: BTree[Nothing] = throw NoSuchElementException()

  override def right: BTree[Nothing] = throw NoSuchElementException()

  override val isEmpty: Boolean = true

  override val isLeaf: Boolean = false

  override val size: Int = 0

  override def collectLeaves: List[BTree[Nothing]] = List.empty

  override def collectNodes(level: Int): List[BTree[Nothing]] = List.empty

  override val mirror: BTree[Nothing] = this

end BEnd

case class BNode[+T](
    override val value: T,
    override val left: BTree[T],
    override val right: BTree[T]
) extends BTree[T]:

  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = (left == null || left.isEmpty) && (right == null || right.isEmpty)

  // the easiest and most elegant solution, but not stack safe
  // override def size: Int = 1 + left.size + right.size

  // recursive solution
  // override def size: Int =
  //   def recurse(nodes: List[BTree[T]], acc: Int = 0): Int =
  //     if nodes.isEmpty then acc
  //     else if nodes.head.isEmpty then recurse(nodes.tail, acc)
  //     else if nodes.head.isLeaf then recurse(nodes.tail, acc + 1)
  //     else
  //       val node = nodes.head
  //       recurseG(node.left :: node.right :: nodes.tail, acc + 1)
  //   recurse(List(this))

  // or... just make it a val
  override val size: Int = 1 + left.size + right.size

  override def collectLeaves: List[BTree[T]] =
    @tailrec
    def collectRecursively(todo: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] =
      if todo.isEmpty then leaves
      else if todo.head.isEmpty then collectRecursively(todo.tail, leaves)
      else if todo.head.isLeaf then collectRecursively(todo.tail, todo.head :: leaves)
      else
        val node = todo.head
        collectRecursively(node.left :: node.right :: todo.tail, leaves)

    collectRecursively(List(this), List.empty)

  override def collectNodes(level: Int): List[BTree[T]] =
    @tailrec
    def recurseG(nodes: List[BTree[T]], currentLevel: Int): List[BTree[T]] =
      if currentLevel == 0 then nodes
      else
        val lowerLevelNodes = nodes
          .filterNot(n => n.isEmpty || n.isLeaf)
          .flatMap(n => List(n.left, n.right))
          .filterNot(n => n.isEmpty)
        recurseG(lowerLevelNodes, currentLevel - 1)

    @tailrec
    def recurseDan(currentLevel: Int, currentNodes: List[BTree[T]]): List[BTree[T]] =
      if currentNodes.isEmpty then List.empty
      else if currentLevel == level then currentNodes
      else
        val expandedNodes =
          for
            node  <- currentNodes
            child <- List(node.left, node.right) if !child.isEmpty
          yield child

        recurseDan(currentLevel + 1, expandedNodes)

    // recurseDan(0, List(this))
    recurseG(List(this), level)

  end collectNodes

  override val mirror: BTree[T] =
    // Complexity: O(N)
    @tailrec
    def recurseDan(
        todo: List[BTree[T]],
        visited: Set[BTree[T]],
        done: List[BTree[T]]
    ): BTree[T] =
      if todo.isEmpty then done.head
      else
        val node = todo.head
        if node.isEmpty || node.isLeaf then recurseDan(todo.tail, visited, node :: done)
        else if !visited.contains(node) then
          recurseDan(node.left :: node.right :: todo, visited + node, done)
        else
          val newLeft  = done.head
          val newRight = done.tail.head
          val newNode  = BNode(node.value, newLeft, newRight)
          recurseDan(todo.tail, visited, newNode :: done.drop(2))

    // my implementation, elegant but stack unsafe -
    // blows up with StackOverflowError if the tree is large enough
    // this.copy(left = right.mirror, right = left.mirror)
    recurseDan(List(this), Set.empty, List.empty)

end BNode

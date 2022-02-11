package trees

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.collection.immutable.Queue

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
  def sameShapeAs[S >: T](that: BTree[S]): Boolean
  def isSymmetrical: Boolean = sameShapeAs(mirror)
  def toList: List[T]
end BTree

case object BEmpty extends BTree[Nothing]:
  override def value: Nothing = throw NoSuchElementException()
  override def left: BTree[Nothing] = throw NoSuchElementException()
  override def right: BTree[Nothing] = throw NoSuchElementException()
  override val isEmpty: Boolean = true
  override val isLeaf: Boolean = false
  override val size: Int = 0
  override def collectLeaves: List[BTree[Nothing]] = List.empty
  override def collectNodes(level: Int): List[BTree[Nothing]] = List.empty
  override val mirror: BTree[Nothing] = this
  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty
  override def toList: List[Nothing] = List.empty
end BEmpty

case class BNode[+T](
    override val value: T,
    override val left: BTree[T] = BEmpty,
    override val right: BTree[T] = BEmpty
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

  // or... just make it a val AND recursive
  override val size: Int = 1 + left.size + right.size

  override def collectLeaves: List[BTree[T]] =
    @tailrec
    def collectRecursively(todo: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] =
      if todo.isEmpty then leaves
      else if todo.head.isEmpty then collectRecursively(todo.tail, leaves)
      else if todo.head.isLeaf then collectRecursively(todo.tail, todo.head :: leaves)
      else collectRecursively(todo.head.left :: todo.head.right :: todo.tail, leaves)

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
            node <- currentNodes
            child <- List(node.left, node.right) if !child.isEmpty
          yield child

        recurseDan(currentLevel + 1, expandedNodes)

    // recurseDan(0, List(this))
    recurseG(List(this), level)

  end collectNodes

  override def mirror: BTree[T] =
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
          val newLeft :: newRight :: _ = done
          val newNode = BNode(node.value, newLeft, newRight)
          recurseDan(todo.tail, visited, newNode :: done.drop(2))

    // my implementation, elegant but stack unsafe -
    // blows up with StackOverflowError if the tree is large enough
    // this.copy(left = right.mirror, right = left.mirror)
    // For some reason, this blows up too. Too lazy right now to find out why...
    // recurseDan(List(this), Set.empty, List.empty)
    this

  end mirror

  override def sameShapeAs[S >: T](that: BTree[S]): Boolean =

    @tailrec
    def recurseG(these: List[BTree[S]], those: List[BTree[S]]): Boolean =
      if these.length != those.length then false
      else if these.forall(_.isLeaf) && those.forall(_.isLeaf) then true
      else
        val nextLevelOfThese = these.filterNot(_.isEmpty).flatMap(n => List(n.left, n.right))
        val nextLevelOfThose = those.filterNot(_.isEmpty).flatMap(n => List(n.left, n.right))

        recurseG(nextLevelOfThese, nextLevelOfThese)

    @tailrec
    def recurseDan(these: List[BTree[S]], those: List[BTree[S]]): Boolean =
      if these.isEmpty then those.isEmpty
      else if those.isEmpty then these.isEmpty
      else
        val thisNode = these.head
        val thatNode = those.head

        if thisNode.isEmpty then thatNode.isEmpty && recurseDan(these.tail, those.tail)
        else if thisNode.isLeaf then thatNode.isLeaf && recurseDan(these.tail, those.tail)
        else
          recurseDan(
            thisNode.left :: thisNode.right :: these.tail,
            thatNode.left :: thatNode.right :: those.tail
          )

    recurseDan(List(this), List(that))

  /**
   * List traversals:
   * - pre-order
   * - in-order
   * - post-order
   * - per-level
   */
  override def toList: List[T] =
    // stack recursive
    // pre-order
    // value :: (left.toList ++ right.toList)
    // in-order
    // left.toList ++ (value :: right.toList)
    // post-order
    // ???
    // pre-order tail recursive
    @tailrec
    def recurseG(nodes: List[BTree[T]], acc: List[T] = List.empty): List[T] =
      if nodes.isEmpty then acc
      else if nodes.head.isEmpty then recurseG(nodes.tail, acc)
      else recurseG(nodes.head.left :: nodes.head.right :: nodes.tail, nodes.head.value :: acc)

    def preOrderStackrec(tree: BTree[T]): List[T] =
      if tree.isEmpty then List.empty
      else tree.value :: preOrderStackrec(tree.left) ++ preOrderStackrec(tree.right)

    @tailrec
    def preOrderTailrec(
        stack: List[BTree[T]],
        visited: Set[BTree[T]] = Set.empty,
        acc: Queue[T] = Queue.empty
    ): List[T] =
      if stack.isEmpty then acc.toList
      else
        val node = stack.head
        if node.isEmpty then preOrderTailrec(stack.tail, visited, acc)
        else if node.isLeaf || visited.contains(node) then
          preOrderTailrec(stack.tail, visited, acc :+ node.value)
        else
          // this would become in-order bei changing the order of prepends to the stack to:
          // node.left :: node :: node.right :: stack.tail
          // same applies to any other order
          preOrderTailrec(node :: node.left :: node.right :: stack.tail, visited + node, acc)

    @tailrec
    def perLevelTailrec(level: List[BTree[T]], finalQueue: Queue[BTree[T]] = Queue.empty): List[T] =
      if level.isEmpty then finalQueue.map(_.value).toList
      else
        perLevelTailrec(
          level.flatMap(node => List(node.left, node.right).filterNot(_.isEmpty)),
          finalQueue ++ level
        )

    // recurseG(List(this)).reverse
    // preOrderStackrec(this)
    perLevelTailrec(List(this))

end BNode

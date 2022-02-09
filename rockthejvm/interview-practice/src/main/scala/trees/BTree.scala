package trees

import java.util.NoSuchElementException
import scala.annotation.tailrec

sealed abstract class BTree[+T]:
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int = collectLeaves.length
  def size: Int
end BTree

case object BEnd extends BTree[Nothing]:
  override def value: Nothing                      = throw NoSuchElementException()
  override def left: BTree[Nothing]                = throw NoSuchElementException()
  override def right: BTree[Nothing]               = throw NoSuchElementException()
  override def isEmpty: Boolean                    = true
  override def isLeaf: Boolean                     = false
  override def collectLeaves: List[BTree[Nothing]] = List.empty
  override val size: Int                           = 0

end BEnd

case class BNode[+T](
    override val value: T,
    override val left: BTree[T],
    override val right: BTree[T]
) extends BTree[T]:
  override def isEmpty: Boolean = false
  override def isLeaf: Boolean  = left == null || left.isEmpty && right == null || right.isEmpty

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

end BNode

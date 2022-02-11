package trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PathSum:

  def hasPathSumG(tree: BTree[Int], target: Int): Boolean =
    !tree.isEmpty &&
      (tree.isLeaf && tree.value == target ||
        hasPathSumG(tree.left, target - tree.value) ||
        hasPathSumG(tree.right, target - tree.value))

  @tailrec
  def hasPathSumStackrec(tree: BTree[Int], target: Int): Boolean =
    if tree.isEmpty then target == 0
    else if tree.isLeaf then tree.value == target
    else if tree.left.isEmpty then hasPathSumStackrec(tree.right, target - tree.value)
    else hasPathSumStackrec(tree.left, target - tree.value)

  // Complexity: O(N) time, O(N) mem
  @tailrec
  def hasPathSumTailrec(nodes: Queue[BTree[Int]], targets: Queue[Int]): Boolean =
    if nodes.isEmpty then false
    else
      val node = nodes.head
      val target = targets.head

      if node.isLeaf && node.value == target then true
      else
        val children = List(node.left, node.right).filterNot(_.isEmpty)
        val childrenTargets = children.map(_ => target - node.value)

        hasPathSumTailrec(nodes.tail ++ children, targets.tail ++ childrenTargets)

  // all the paths from root to leaf such that the sum of values == target
  def findSumPaths(tree: BTree[Int], target: Int): List[List[Int]] =
    def stackrec(currentTree: BTree[Int] = tree, currentTarget: Int = target): List[List[Int]] =
      if currentTree.isEmpty then List.empty
      else if currentTree.isLeaf then
        if currentTree.value == currentTarget then List(List(currentTree.value))
        else List.empty
      else
        List(currentTree.left, currentTree.right)
          .filterNot(_.isEmpty)
          .flatMap { child =>
            stackrec(child, currentTarget - currentTree.value).map(currentTree.value :: _)
          }

    @tailrec
    def tailrec(
        nodes: List[BTree[Int]] = List(tree),
        targets: List[Int] = List(target),
        currentPath: List[BTree[Int]] = List.empty,
        expanded: Set[BTree[Int]] = Set.empty,
        acc: List[List[Int]] = List.empty
    ): List[List[Int]] =
      if nodes.isEmpty then acc
      else
        val currentNode = nodes.head
        val currentTarget = targets.head

        if currentNode.isLeaf then
          if currentNode.value == currentTarget then
            tailrec(
              nodes.tail,
              targets.tail,
              currentPath,
              expanded,
              (currentNode :: currentPath).reverse.map(_.value) :: acc
            )
          else tailrec(nodes.tail, targets.tail, currentPath, expanded, acc)
        else if expanded.contains(currentNode) then
          tailrec(nodes.tail, targets.tail, currentPath.tail, expanded, acc)
        else
          val children = List(currentNode.left, currentNode.right).filterNot(_.isEmpty)
          val childrenTargets = children.map(_ => currentTarget - currentNode.value)
          tailrec(
            children ++ nodes.tail,
            childrenTargets ++ targets,
            currentNode :: currentPath,
            expanded + currentNode,
            acc
          )

    tailrec()

package trees

import scala.annotation.tailrec

object CameraSurveillance:
  enum SurveillanceState:
    case COVERED, NOT_COVERED, CAMERA

  /**
   * Given a binary tree, we install cameras on the nodes of the trees.
   * Each camera at a node can monitor its parent, itself, and its immediate children.
   * Calculate the minimum number of cameras needed to monitor all nodes of the tree.
   * 
   * @param tree The tree to monitor
   * @tparam T The value type of the tree
   * @return The minimum number of cameras needed to monitor all nodes of the tree
   */
  def solution[T](tree: BTree[T]): Int =

    def stackrec(node: BTree[T]): (Int, SurveillanceState) =
      if node.isEmpty then (0, SurveillanceState.COVERED)
      else
        val (leftNumCameras, leftState) = stackrec(node.left)
        val (rightNumCameras, rightState) = stackrec(node.right)

        if leftState == SurveillanceState.NOT_COVERED || rightState == SurveillanceState.NOT_COVERED then
          (leftNumCameras + rightNumCameras + 1, SurveillanceState.CAMERA)
        else if leftState == SurveillanceState.CAMERA || rightState == SurveillanceState.CAMERA then
          (leftNumCameras + rightNumCameras, SurveillanceState.COVERED)
        else (leftNumCameras + rightNumCameras, SurveillanceState.NOT_COVERED)

    @tailrec
    def tailrec(
        stack: List[BTree[T]],
        visited: Set[BTree[T]],
        covered: List[(Int, SurveillanceState)]
    ): (Int, SurveillanceState) =
      if stack.isEmpty then covered.head
      else
        val currentNode = stack.head
        if currentNode.isEmpty then
          tailrec(stack.tail, visited, (0, SurveillanceState.COVERED) :: covered)
        else if !visited.contains(currentNode) then
          tailrec(currentNode.left :: currentNode.right :: stack, visited + currentNode, covered)
        else
          val (leftNumCameras, leftState) = covered.head
          val (rightNumCameras, rightState) = covered.tail.head
          val parentState =
            if leftState == SurveillanceState.NOT_COVERED || rightState == SurveillanceState.NOT_COVERED then
              (leftNumCameras + rightNumCameras + 1, SurveillanceState.CAMERA)
            else if leftState == SurveillanceState.CAMERA || rightState == SurveillanceState.CAMERA then
              (leftNumCameras + rightNumCameras, SurveillanceState.COVERED)
            else (leftNumCameras + rightNumCameras, SurveillanceState.NOT_COVERED)

          tailrec(stack.tail, visited, parentState :: covered)

    val (cams, state) = stackrec(tree)
    if state == SurveillanceState.NOT_COVERED then cams + 1
    else cams

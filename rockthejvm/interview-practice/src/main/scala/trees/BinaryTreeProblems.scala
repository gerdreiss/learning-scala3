package trees

import scala.collection.immutable.Queue

object BinaryTreeProblems extends App:

  // @formatter:off
  val tree =
    BNode(1,
      BNode(2, 
        BNode(3), 
        BNode(4, 
          BEmpty, 
          BNode(5)
        )
      ),
      BNode(6, 
        BNode(7), 
        BNode(8)
      )
    )
  val tree2 =
    BNode(1,
      BNode(2, 
        BNode(3), 
        BNode(4, 
          BNode(5), 
          BNode(6)
        )
      ),
      BNode(7, 
        BNode(8), 
        BNode(9)
      )
    )
  val mirrored =
    BNode(1,
      BNode(6, 
        BNode(8),
        BNode(7)
      ),
      BNode(2, 
        BNode(4, 
          BNode(5),
          BEmpty
        ),
        BNode(3)
      )
    )
    // @formatter:on

  // println(tree.leafCount)
  // println(tree.collectLeaves)
  // println(tree.size)

  // val degenerate = (1 to 10000000).foldLeft[BTree[Int]](BEmpty)((tree, n) => BNode(n, tree, BEmpty))
  // val degenerate2 = (1 to 10000000).foldRight[BTree[Int]](BEmpty)((n, tree) => BNode(n, tree, BEmpty))
  // if size were a def , this would blow up with StackOverflowError
  // println(degenerate.size)
  // this does blow up independent of being a val or a def, if implemented stack recursively
  // println(degenerate.mirror.size)

  // println(tree.collectNodes(0).map(_.value))
  // println(tree.collectNodes(1).map(_.value))
  // println(tree.collectNodes(2).map(_.value))
  // println(tree.collectNodes(3).map(_.value))
  // println(tree.collectNodes(10).map(_.value))

  // println(tree)
  // println(tree.mirror)

  // println(tree.sameShapeAs(tree))
  // println(tree.sameShapeAs(tree2))
  // println(tree.toList)

  // println("=" * 100)
  // println(PathSum.hasPathSumG(tree, 6))
  // println(PathSum.hasPathSumG(tree, 7))
  // this one fails with StackOverflowError
  // println(PathSum.hasPathSumG(degenerate, 100))
  // println("StackOverflowError")

  // println("=" * 100)
  // println(PathSum.hasPathSumStackrec(tree, 6))
  // println(PathSum.hasPathSumStackrec(tree, 7))
  // println(PathSum.hasPathSumStackrec(degenerate, 100))

  // println("=" * 100)
  // println(PathSum.hasPathSumTailrec(Queue(tree), Queue(6)))
  // println(PathSum.hasPathSumTailrec(Queue(tree), Queue(7)))
  // println(PathSum.hasPathSumTailrec(Queue(degenerate), Queue(100)))

  println("=" * 100)
  println(PathSum.findSumPaths(tree, 6))
  println(PathSum.findSumPaths(tree, 14))

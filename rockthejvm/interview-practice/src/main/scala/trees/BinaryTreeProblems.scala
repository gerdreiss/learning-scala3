package trees

object BinaryTreeProblems extends App:

  val tree =
    // @formatter:off
    BNode(1,
      BNode(2, 
        BNode(3, BEnd, BEnd), 
        BNode(4, 
          BEnd, 
          BNode(5, BEnd, BEnd)
        )
      ),
      BNode(6, 
        BNode(7, BEnd, BEnd), 
        BNode(8, BEnd, BEnd)
      )
    )
    // @formatter:on

  // println(tree.leafCount)
  // println(tree.collectLeaves)
  // println(tree.size)

  // val degenerate = (1 to 10000000).foldLeft[BTree[Int]](BEnd)((tree, n) => BNode(n, tree, BEnd))
  // if size were a def , this would blow up with StackOverflowError
  // println(degenerate.size)

  println(tree.collectNodes(0).map(_.value))
  println(tree.collectNodes(1).map(_.value))
  println(tree.collectNodes(2).map(_.value))
  println(tree.collectNodes(3).map(_.value))
  println(tree.collectNodes(10).map(_.value))

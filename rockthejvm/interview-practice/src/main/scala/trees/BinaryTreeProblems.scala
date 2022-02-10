package trees

object BinaryTreeProblems extends App:

  // @formatter:off
  val tree =
    BNode(1,
      BNode(2, 
        BNode(3, BEmpty, BEmpty), 
        BNode(4, 
          BEmpty, 
          BNode(5, BEmpty, BEmpty)
        )
      ),
      BNode(6, 
        BNode(7, BEmpty, BEmpty), 
        BNode(8, BEmpty, BEmpty)
      )
    )
  val tree2 =
    BNode(1,
      BNode(2, 
        BNode(3, BEmpty, BEmpty), 
        BNode(4, 
          BNode(5, BEmpty, BEmpty), 
          BNode(6, BEmpty, BEmpty)
        )
      ),
      BNode(7, 
        BNode(8, BEmpty, BEmpty), 
        BNode(9, BEmpty, BEmpty)
      )
    )
  val mirrored =
    BNode(1,
      BNode(6, 
        BNode(8, BEmpty, BEmpty),
        BNode(7, BEmpty, BEmpty)
      ),
      BNode(2, 
        BNode(4, 
          BNode(5, BEmpty, BEmpty),
          BEmpty
        ),
        BNode(3, BEmpty, BEmpty)
      )
    )
    // @formatter:on

  // println(tree.leafCount)
  // println(tree.collectLeaves)
  // println(tree.size)

  // val degenerate = (1 to 10000000).foldLeft[BTree[Int]](BEnd)((tree, n) => BNode(n, tree, BEnd))
  // if size were a def , this would blow up with StackOverflowError
  // println(degenerate.size)
  // this does blow up independent of being a val or a def, if implemented stack recursively
  // println(degenerate.mirror.size)

  // println(tree.collectNodes(0).map(_.value))
  // println(tree.collectNodes(1).map(_.value))
  // println(tree.collectNodes(2).map(_.value))
  // println(tree.collectNodes(3).map(_.value))
  // println(tree.collectNodes(10).map(_.value))

  // println("=" * 100)
  // println(tree)
  // println(tree.mirror)

  println("=" * 100)
  println(tree.sameShapeAs(tree))
  println(tree.sameShapeAs(tree2))

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

  println(tree.leafCount)
  println(tree.collectLeaves)

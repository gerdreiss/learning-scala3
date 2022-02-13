package graphs

import scala.annotation.tailrec

opaque type Graph[T] = Map[T, Set[T]]
object Graph:
  def empty[T]: Graph[T] = Map.empty
  def edge[T](a: T): Graph[T] = Map(a -> Set.empty)
  def node[T](a: T, associates: Set[T]) = Map(a -> associates)
  def from[T](associations: Map[T, Set[T]]): Graph[T] = associations
  def associate[T](a: T, b: T): Graph[T] = node(a, Set(b))
end Graph

trait Monoid[F[_]]:
  def empty[T]: F[T]
  def combine[T](left: F[T], right: F[T]): F[T]

given Monoid[Set] with
  def empty[T]: Set[T] = Set.empty
  def combine[T](left: Set[T], right: Set[T]): Set[T] = left ++ right

given graphMonoid(using M: Monoid[Set]): Monoid[Graph] with
  def empty[T]: Graph[T] = Graph.empty[T]
  def combine[T](left: Graph[T], right: Graph[T]): Graph[T] =
    left.foldLeft(empty) { case (acc, (node, associates)) =>
      acc + (node -> M.combine(associates, right.get(node).getOrElse(Set.empty)))
    }

extension [T](graph: Graph[T])
  // number of nodes this node 'node' is associated (adjacent) to
  def outDegree(node: T): Int =
    graph.get(node).map(_.size).getOrElse(0)

  // number of nodes connected to 'node'
  def inDegree(node: T): Int =
    graph.values.count(_.contains(node))

  def isPath(from: T, to: T): Boolean =
    @tailrec
    def recurse(remaining: List[T], considered: Set[T] = Set.empty): Boolean =
      if remaining.isEmpty then false
      else
        val currentNode = remaining.head
        // this would work, but would not be tailrec
        // currentNode == to ||
        //   considered.contains(currentNode) && recurse(remaining.tail, considered) ||
        //   recurse(remaining.tail ++ graph(currentNode), considered + currentNode)
        if currentNode == to then true
        else if considered.contains(currentNode) then recurse(remaining.tail, considered)
        else recurse(remaining.tail ++ graph(currentNode), considered + currentNode)

    // stack recursive, will probably result in a StackOverflowError if the graph is large enough
    // inDegree(to) > 0 && graph.get(from).exists(_.exists(ass => ass == to || isPath(ass, to)))
    // calling the tail recursive method from above
    recurse(List(from))
  end isPath

  def findPath(from: T, to: T): List[T] =
    @tailrec
    def recurse(remaining: List[(T, List[T])], considered: Set[T] = Set.empty): List[T] =
      if remaining.isEmpty then List.empty
      else
        val (currentNode, currentPath) = remaining.head
        if currentNode == to then currentPath.reverse
        else if considered.contains(currentNode) then
          if currentNode == to then (currentNode :: currentPath).reverse
          else recurse(remaining.tail, considered)
        else
          recurse(
            remaining.tail ++ graph(currentNode).map(n => (n, n :: currentPath)),
            considered + currentNode
          )

    recurse(graph(from).map(n => (n, n :: List(from))).toList, Set(from))
  end findPath

  def findCycle(node: T): List[T] = findPath(node, node)

  def undirectG(using M: Monoid[Graph]): Graph[T] =
    val undirected = graph.toSet
      .flatMap((node, associates) => associates.map(_ -> node))
      .groupMap(_._1)(_._2)
    M.combine(graph, undirected)

  def undirectDan: Graph[T] =
    def addEdge(intermediate: Graph[T], from: T, to: T): Graph[T] =
      // my variation of the Dan's implementation below
      // graph ++ graph
      //   .get(from)
      //   .map(_ + to)
      //   .fold(Graph.associate(from, to))(Graph.node(from, _))
      // Dan's implementation
      if graph.get(from).isEmpty then graph + (from -> Set(to))
      else
        val associates = graph(from)
        graph + (from -> (associates + to))

    def addOpposingEdges(remaining: Set[T], acc: Graph[T]): Graph[T] =
      if remaining.isEmpty then acc
      else
        val currentNode = remaining.head
        val associates = graph(currentNode)
        val newGraph = associates.foldLeft(acc) { (intermediate, associate) =>
          addEdge(intermediate, associate, currentNode)
        }
        addOpposingEdges(remaining.tail, newGraph)

    addOpposingEdges(graph.keySet, graph)

  def color: Map[T, Int] =
    def recurse(
        remaining: List[T],
        currentColor: Int = 0,
        colors: Map[T, Int] = Map.empty
    ): Map[T, Int] =
      if remaining.isEmpty then colors
      else
        val currentNode = remaining.head
        if colors.contains(currentNode) then recurse(remaining.tail, currentColor, colors)
        else
          val uncolored = remaining.tail.foldLeft[Set[T]](Set(currentNode)) { (acc, next) =>
            val allAssociates = acc.flatMap(graph.undirectDan)
            if colors.contains(next) || allAssociates.contains(next) then acc
            else acc + next
          }
          recurse(remaining.tail, currentColor + 1, colors ++ uncolored.map((_, currentColor)))

    val descending = graph.keySet.toList.sortWith((a, b) => outDegree(a) > outDegree(b))

    recurse(descending)

end extension

object GraphProblems extends App:

  val socialNetwork: Graph[String] = Graph.from(
    Map(
      "Alice" -> Set("Bob", "Charlie", "David"),
      "Bob" -> Set.empty,
      "Charlie" -> Set("David"),
      "David" -> Set("Bob", "Mary"),
      "Mary" -> Set("Bob", "Charlie")
    )
  )
  val undirected = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set("Alice", "Mary", "David"),
    "Charlie" -> Set("David", "Alice", "Mary"),
    "Mary" -> Set("Bob", "Charlie", "David"),
    "David" -> Set("Bob", "Mary", "Alice", "Charlie")
  )

  // println(socialNetwork.outDegree("Alice")) // 3
  // println(socialNetwork.inDegree("David")) // 2
  // println(socialNetwork.isPath("Mary", "Alice")) // false
  // println(socialNetwork.findPath("Alice", "Mary")) // [Alice, David, Mary]
  // println(socialNetwork.findCycle("Charlie")) // [Charlie, David, Mary, Charlie]
  // println(socialNetwork.undirectDan)
  println(socialNetwork.undirectG)

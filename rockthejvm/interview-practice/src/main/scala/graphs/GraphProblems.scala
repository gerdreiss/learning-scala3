package graphs

import scala.annotation.tailrec

opaque type Graph[T] = Map[T, Set[T]]
object Graph:
  def empty[T]: Graph[T] = Map.empty
  def edge[T](a: T): Graph[T] = Map(a -> Set.empty)
  def node[T](a: T, associates: Set[T]) = Map(a -> associates)
  def from[T](associations: Map[T, Set[T]]): Graph[T] = associations
end Graph

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
    recurse(List(from))

  def findPath(from: T, to: T): Set[T] = ???

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

  println(socialNetwork.outDegree("Alice")) // 3
  println(socialNetwork.inDegree("David")) // 2
  println(socialNetwork.isPath("Mary", "Alice")) // false

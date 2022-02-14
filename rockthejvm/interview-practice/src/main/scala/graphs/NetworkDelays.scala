package graphs

opaque type Node = Int
opaque type Delay = Int

/**
 * n - number of nodes
 * times  = list((a,b,t))
 * (a,b,t) = time between node a and node b is t
 */
def computeNetworkDelay(n: Int, times: List[(Node, Node, Delay)], source: Int): Delay =
  // adjacency list/graph
  val graph = times.toSet.groupMap(_._1)(_._2)
  // adjacency matrix
  val weights: Map[(Node, Node), Delay] = times.map((a, b, t) => (a, b) -> t).toMap

  def dijkstra(
      expanding: Set[Node],
      visited: Set[Node],
      costs: Map[Node, Delay]
  ): Map[Node, Delay] =
    if expanding.isEmpty then costs
    else
      val currentNode = expanding.minBy(costs)
      val nbrCosts = graph
        .getOrElse(currentNode, Set.empty)
        .map { nbr =>
          val currentCost = costs.getOrElse(nbr, Int.MaxValue)
          val tentativeCost = costs(currentNode) + weights((currentNode, nbr))
          val bestCost = currentCost.min(tentativeCost)

          (nbr, bestCost)
        }
        .toMap

      val unvisited = nbrCosts.keySet.filterNot(visited)
      dijkstra(expanding - currentNode ++ unvisited, visited + currentNode, costs ++ nbrCosts)

  val initialCosts = (1 to n).map((_, Int.MaxValue)).toMap + (source -> 0)
  val latencies = dijkstra(Set(source), Set.empty, initialCosts)
  val maxLatency = latencies.values.max

  if maxLatency == Int.MaxValue then -1
  else maxLatency

object NetworkDelays extends App {}

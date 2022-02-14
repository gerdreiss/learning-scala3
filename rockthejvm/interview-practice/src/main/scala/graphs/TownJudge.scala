package graphs

import scala.util.Random

/**
 * n people, 1 to n
 * trust -> first trusts the second
 * 
 * There might b a town judge.
 *   The down judge trusts nobody.
 *   Everybody (except for the town judge) trusts the town judge.
 *   There is exactly on person that satisfies these properties.
 * 
 *  Find the town judge, or return -1.
 */
val nobody = -1

def findJudgeG(n: Int, trust: List[(Int, Int)]): Int =
  val graph = Graph.from(trust.toSet.groupMap(_._1)(_._2))
  (1 to n)
    .find(candidate => graph.outDegree(candidate) == 0 && graph.inDegree(candidate) == n - 1)
    .getOrElse(nobody)

def findJudgeDan(n: Int, trust: List[(Int, Int)]): Int =
  val inDegrees = trust.foldLeft(Map.empty[Int, Int]) { case (acc, (_, b)) =>
    acc + (b -> (acc.getOrElse(b, 0) + 1))
  }
  val outDegrees = trust.foldLeft(Map.empty[Int, Int]) { case (acc, (a, b)) =>
    acc + (a -> (acc.getOrElse(a, 0) + 1))
  }

  (1 to n)
    .find(p => inDegrees.getOrElse(p, 0) == n - 1 && outDegrees.getOrElse(p, 0) == 0)
    .getOrElse(nobody)

object TownJudge extends App:

  val rnd = new Random

  val numPeople = 10
  val judge = 3

  val peopleWithoutJudge = (1 to numPeople).filterNot(_ == judge).toList

  println(
    findJudgeG(
      numPeople,
      peopleWithoutJudge.map((_, judge)).toList
    )
  )

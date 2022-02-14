package graphs

/**
 * nCourses: 0 to n - 1
 * prereqs: (a, b) means b is required to take a
 */
def canTakeAllCoursesG(nCourses: Int, prereqs: List[(Int, Int)]): Boolean =
  val allCourses = (0 until nCourses).map((_, Set.empty[Int])).toMap
  val allPrereqs = prereqs.toSet.groupMap(_._1)(_._2)
  val graph = Graph.from(allCourses ++ allPrereqs)
  (0 until nCourses).forall(course => graph.findCycle(course).isEmpty)

def canTakeAllCoursesDan(nCourses: Int, prereqs: List[(Int, Int)]): Boolean =
  val dependencies =
    (0 until nCourses).map((_, Set.empty[Int])).toMap ++
      prereqs.foldLeft(Map.empty[Int, Set[Int]]) { case (acc, (a, b)) =>
        acc + (a -> (acc.getOrElse(a, Set.empty) + b))
      }
  val graph: Graph[Int] = Graph.from(dependencies)

  (0 until nCourses).forall(course => graph.findCycle(course).isEmpty)

object UniCourses extends App:

  println(canTakeAllCoursesDan(2, List((0, 1))))
  println(canTakeAllCoursesG(2, List((0, 1))))

  println(canTakeAllCoursesDan(2, List((0, 1), (1, 0))))
  println(canTakeAllCoursesG(2, List((0, 1), (1, 0))))

  println(canTakeAllCoursesDan(5, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4))))
  println(canTakeAllCoursesG(5, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4))))

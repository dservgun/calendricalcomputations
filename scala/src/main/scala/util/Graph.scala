package util.Graph 

object GraphUtils extends scala.App {
  type AdjacencyList = Map[Int, Set[Int]]
  //(perm, temp, accum)
  type Visitor = (Set[Int], Set[Int], Set[(Int, Int)])
  val initVisitor : Visitor = (Set(),Set(), Set())

  case class Graph(adjList : AdjacencyList) {
    def addNode(aNode : Int) : AdjacencyList = {
      adjList + (aNode -> Set())
    }

    def addDependency(from : Int, to : Int) : Graph = {
      val currentVal : Set[Int] = adjList.get(to).getOrElse(Set())
      Graph(adjList + (from -> (currentVal + to)))
    }
    def combineWithBias(visitor : Visitor, visitor2 : Visitor) : Visitor = {
      (visitor._1 ++ visitor2._1
        , visitor._2 ++ visitor2._2
        , visitor._3.foldLeft(visitor2._3) {
            (result, element) => shiftInsert(element._2, result)
          }) // TODO: fix this
    }

    def shiftInsert (aNode : Int, currentSet : Set[(Int, Int)]) : Set[(Int, Int)] = {
      val exists = currentSet.filter(ele => ele._2 == aNode).nonEmpty
      if (exists) currentSet 
      else (Set((1, aNode)) ++ (currentSet.map((ele) => (ele._1 + 1, ele._2))))
    } 
    
    def visit(node : Int, visitor : Visitor) : Visitor = {
      if (visitor._1.contains(node)) {
        visitor 
      }
      if (visitor._2.contains(node)) {
        sys.error("Cycle.")
      }
      //for each node , visit and accumulate
      val edges = adjList.get(node).getOrElse(Set())
      val accRes = edges.foldLeft (visitor) {
        (result, element) => combineWithBias(result, visit(element, result))
      }
      //add self to the permanent node.
      (accRes._1 ++ Set(node), accRes._2, shiftInsert(node, accRes._3))
    }
    def dfs : Visitor = {
      adjList.foldLeft(initVisitor) {
        (result, element) => {
          println(result._3)
          visit(element._1, result)
        }
      }
    }
  } //caseclass

  val g = Graph(Map(
      3 -> Set(8, 10)
      , 5 -> Set(11)
      , 7 -> Set(11, 8)
      , 11 -> Set(10, 9, 2)))
  println(g.adjList)
  println(g.dfs._3)

}
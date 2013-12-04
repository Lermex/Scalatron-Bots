import scala.collection.immutable.List
import scala.collection.mutable.PriorityQueue

case class Node (xy: XY, var previous: Node, var hCost: Int, var gCost: Int) {
  override def equals(obj:Any) = obj.isInstanceOf[Node] && obj.asInstanceOf[Node].xy == this.xy
}

object AStar {

  object Heuristic extends Ordering[Node] {
    def compare(first: Node, second: Node) = (second.gCost + second.hCost).compare(first.gCost + first.hCost)
  }

  // find shortest path to goal or return None if it is unreachable
  def apply(start: XY, goal: XY, vision: Vision): Option[List[Node]] = {
    var closedList: List[Node] = Nil
    val openList = new PriorityQueue[Node]()(Heuristic)

    val startNode = new Node(start, null, Int.MaxValue, 0)
    openList.enqueue(startNode)

    while (openList.nonEmpty) {
      val current = openList.dequeue()
      closedList = current :: closedList

      if (current.xy == goal) {
        return Some(reconstructPath(startNode, current))
      } else {
        for (adjacentNode <- getAdjacent(current, vision)) {
          processAdjacentNode(adjacentNode, current, openList, closedList, goal, vision)
        }
      }
    }
    None
  }

  def processAdjacentNode(adjacentNode: Node,
                          current: Node,
                          openList: PriorityQueue[Node],
                          closedList: List[Node],
                          goal: XY,
                          vision: Vision): Unit = {

      if (!isWalkable(adjacentNode, vision) || closedList.contains(adjacentNode)) return

      openList.find (x => x == adjacentNode) match {
        case Some(elem: Node) => {
          if (adjacentNode.gCost > current.gCost + 1) {
            adjacentNode.previous = current
            adjacentNode.gCost = current.gCost + 1

          }
        }

        case None => {
          adjacentNode.previous = current
          adjacentNode.hCost = calculateHCost(adjacentNode, goal)
          adjacentNode.gCost = current.gCost + 1
          openList.enqueue(adjacentNode)
        }
      }
  }

  def calculateHCost(node: Node, goal: XY): Int = {
    val dx = node.xy.x - goal.x
    val dy = node.xy.y - goal.y

    math.sqrt((dx*dx)+(dy*dy)).toInt
  }

  def getAdjacent(node : Node, vision: Vision) : List[Node] = {
    val x = node.xy.x
    val y = node.xy.y
    var adjacent: List[Node] = List()

    if (x != 0 && y != 0) adjacent = adjacent.::(new Node(XY(x-1,y+1), null, Int.MaxValue, node.gCost + 1))
    if (x != 0) adjacent = adjacent.::(new Node(XY(x-1,y), null, Int.MaxValue, node.gCost + 1))
    if (x != 0 && y != vision.size) adjacent = adjacent.::(new Node(XY(x-1,y-1), null, Int.MaxValue, node.gCost + 1))

    if (y != 0) adjacent = adjacent.::(new Node(XY(x,y+1), null, Int.MaxValue, node.gCost + 1))
    if (y != vision.size) adjacent = adjacent.::(new Node(XY(x,y-1), null, Int.MaxValue, node.gCost + 1))

    if (x != vision.size && y != 0) adjacent = adjacent.::(new Node(XY(x+1,y+1), null, Int.MaxValue, node.gCost + 1))
    if (x != vision.size) adjacent = adjacent.::(new Node(XY(x+1,y), null, Int.MaxValue, node.gCost + 1))
    if (x != vision.size && y != vision.size) adjacent = adjacent.::(new Node(XY(x+1,y-1), null, Int.MaxValue, node.gCost + 1))

    adjacent
  }

  def isWalkable(node : Node, vision: Vision) : Boolean = {
    val cell = vision.cellAtAbsPos(node.xy)
    if (cell == 'b' || cell == 'm' || cell == 'p' || cell == 'W') false else true
  }

  def reconstructPath(start: Node, current: Node): List[Node] = {
    if (current == start) {
      List(current)
    } else {
      reconstructPath(start, current.previous).::(current)
    }
  }

}
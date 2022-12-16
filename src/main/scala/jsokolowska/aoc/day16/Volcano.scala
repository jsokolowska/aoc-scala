package jsokolowska.aoc.day16

import scala.collection.mutable

class Volcano:
  case class Valve(id: String, var pressure: Int, connectionIdentifiers: List[String]):
    var connections = List.empty[(Valve, Int)]
    override def toString: String = s"V[$id, $pressure]"

  def partOne(input: List[String]): Int = {
    val lineRe = "Valve ([A-Z]+)[a-z ]*=([0-9]+);[a-z ]*([A-Z, ]+)".r

    val valves = input
      .flatMap(lineRe.findAllMatchIn(_).map(matches =>
        Valve(matches.group(1), matches.group(2).toInt, parseValveList(matches.group(3)))))


    valves.foreach(v => v.connections = valves.filter(
      valve => v.connectionIdentifiers.contains(valve.id)
    ).map((_, 1)))

    val startingPoint = valves.find(_.id == "AA").get

    val usefulNodes = valves.filter(_.pressure > 0).toSet + startingPoint
    val usefulShortestPaths = usefulNodes.map(node => (node, Dijkstra(node, valves).filter((id, *) =>
      usefulNodes.exists(_.id == id)))
    ).toMap

    val noStartNode = usefulNodes.filter(_.id != startingPoint.id)
    noStartNode.map(node => {
      val tCost = usefulShortestPaths.find((k, *) => k.id == startingPoint.id).get._2(node.id)
      maxPressure(30 - tCost, node.id, noStartNode.map(_.id) - node.id, usefulShortestPaths)
    }
    ).max
  }

  private def parseValveList(valveList: String): List[String] = valveList.split(",").map(_.trim).toList

  private def Dijkstra(source: Valve, valves: List[Valve]): Map[String, Int] = {
    var distances = valves.map(v => (v.id, Int.MaxValue)).toMap + (source.id -> 0)
    val knownShortestPaths = mutable.Set[String]()
    while (knownShortestPaths.size < valves.size) {
      val (nodeId, distance) = distances
        .filter((k, v) => v != Int.MaxValue & !knownShortestPaths.contains(k)).toList.minBy((*, k) => k)
      valves.find(_.id == nodeId).get.connections.filter((v, *) => !knownShortestPaths.contains(v.id)).foreach(
        (neigh, *) => {
          val currDist = distances(neigh.id)
          val updatedDist = distance + 1
          distances = distances + (neigh.id -> List(currDist, updatedDist).min)
        }
      )
      knownShortestPaths.add(nodeId)
    }
    distances
  }
  private def maxPressure(time: Int, currentNode: String, unusedNodes: Set[String], shortestPaths: Map[Valve, Map[String, Int]]): Int = {
    if (time <= 1) {
      0
    } else {
      val pathsForCurrentNode = shortestPaths.find((k, *) => k.id == currentNode).get
      val currNode = pathsForCurrentNode._1
      val partialPressure = currNode.pressure * (time - 1)

      if (unusedNodes.nonEmpty) {
        partialPressure + unusedNodes.map(nextNode => {
          val travelCost = pathsForCurrentNode._2(nextNode)
          maxPressure(time - travelCost - 1, nextNode, unusedNodes - nextNode, shortestPaths)
        }).max
      } else {
        partialPressure
      }
    }
  }

end Volcano



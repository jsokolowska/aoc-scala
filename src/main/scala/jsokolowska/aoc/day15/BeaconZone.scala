package jsokolowska.aoc.day15

import scala.collection.mutable

class BeaconZone {
  var busyPoints: mutable.Set[Int] = mutable.Set.empty;

  def partOne(input: List[String], rowId:Int): Int = {
    val lineRe = "Sensor at x=([0-9]+), y=([0-9]+): closest beacon is at x=([0-9]+), y=([0-9]+)".r

    val beaconPoints = input
        .flatMap(lineRe.findAllMatchIn(_).map(matches => ((matches.group(1).toInt, matches.group(2).toInt), (matches.group(3).toInt, matches.group(4).toInt))))
    var c = 0;


    busyPoints.size
  }

  private def dist (point: (Int, Int), beacon: (Int, Int)) = (point._1 - beacon._1).abs + (point._2 - beacon._2).abs
  private def findExclusiveZone(point: (Int, Int), beacon: (Int, Int), rowId: Int) = {
    val beaconPointDistance = (point._1 - beacon._1).abs + (point._2 - beacon._2).abs
    val pointRowDistance = (point._2 - rowId).abs
    val diff = beaconPointDistance - pointRowDistance

    val newP = 0.to(diff).flatMap(diffAm => List(point._1 + diffAm , point._1 - diffAm)).toList
    busyPoints.addAll(newP)
    if(beacon._2 == rowId) {
      busyPoints = busyPoints.diff(Set(beacon._1))
    }
    if(point._2 == rowId) busyPoints = busyPoints + point._1
  }

}

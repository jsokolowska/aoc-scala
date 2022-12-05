package jsokolowska.aoc.day04

import scala.util.Try

class CampAssignment {
  //   9) 409 ****  jsokolowska
  def findProblematicAssignments(assignments: Try[List[String]]): Int = {
    assignments.get.map(_.split(",")).map(pair => {
      pair.flatMap(_.split('-'))
    }).count(overlap)
  }

  private def overlap(pairs: Array[String]): Boolean = {
    val startFirst = pairs.head.toInt
    val endFirst = pairs(1).toInt
    val startSecond = pairs(2).toInt
    val endSecond = pairs(3).toInt
    !( endFirst < endSecond && endFirst < startSecond|| endSecond < endFirst && endSecond < startFirst)
  }

  private def containment(pairs: Array[String]): Boolean = {
    val startFirst = pairs.head.toInt
    val endFirst = pairs(1).toInt
    val startSecond = pairs(2).toInt
    val endSecond = pairs(3).toInt
    !(endFirst <= endSecond && startFirst >= startSecond || endSecond <= endFirst && startSecond >= startFirst)
  }


}

package jsokolowska.aoc.day03

import scala.util.Try

class Backpack {
  def calculateMisplacementCost(backpacks: Try[List[String]]): Int = {
    backpacks.get.map(backpack => {
      val mid = backpack.length / 2
      val comp1 = backpack.substring(0, mid)
      val comp2 = backpack.substring(mid)
      val common = comp1 intersect comp2
      val c1 = common.charAt(0)
      mapCost(c1)
    }).sum
  }

  def mapCost(char: Char): Int = {
    if( char <= 'z' && char >= 'a'){
      return char - 'a'+1
    }
    char - 'A' + 27
  }

  def getBadges(backpacks: Try[List[String]]): Int ={
    backpacks.get.grouped(3).map(
      group => group.head intersect group(1) intersect group(2)
    ).map(same => mapCost(same.charAt(0))).sum
  }

}

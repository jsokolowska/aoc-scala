package jsokolowska.aoc.day01

import scala.util.Try

class Calorie {
  def countMaxCalories(calorieList: Try[List[String]]): Int = {
    var maxCalories = 0
    var currCalories = 0
    for (record <- calorieList.get) {
      if (record.isBlank) {
        maxCalories = maxCalories.max(currCalories)
        currCalories = 0
      } else {
        currCalories += record.toInt
      }
    }
    maxCalories.max(currCalories)
  }

  def countGetTop3Calories(calorieList: Try[List[String]]): Int = {
    var calorieCounts: List[Int] = List()
    var currCalories = 0
    for (record <- calorieList.get) {
      if (record.isBlank) {
        calorieCounts = calorieCounts :+ currCalories
        currCalories = 0
      } else {
        currCalories += record.toInt
      }
    }
    calorieCounts.sorted.reverse.take(3).sum
  }


}

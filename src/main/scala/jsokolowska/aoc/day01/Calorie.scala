package jsokolowska.aoc.day01

import scala.util.Try

class Calorie {
  def countMaxCalories(calorieList: Try[String]): Int = {
   countGetTopN(calorieList,1).sum
  }

  def countGetTopN(calorieList: Try[String], n: Int): Array[Int] = {
    calorieList.get.split("\n\n")
      .map(_.split("\n"))
      .map(lst => lst.map(_.toInt))
      .map(_.sum).sorted.reverse.take(n)
  }

  def countGetTop3Calories(calorieList: Try[String]): Int = {
   countGetTopN(calorieList, 3).sum
  }


}

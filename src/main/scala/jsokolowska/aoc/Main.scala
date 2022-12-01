package jsokolowska.aoc

import jsokolowska.aoc.day01.Calorie

import scala.io.Source
import scala.util.{Try, Using}

object Main {
  val fileName = "src/main/resources/day01/input.txt"

  def main(args: Array[String]): Unit = {
    val cal = new Calorie
    println(cal.countMaxCalories(readAsString(fileName)))
    println(cal.countGetTop3Calories(readAsString(fileName)))
  }

  def readAsString(fileName: String): Try[String]
  =
    Using(Source.fromFile(fileName)) { bufferedSource =>
      bufferedSource.mkString    }
}
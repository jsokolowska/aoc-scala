package jsokolowska.aoc

import jsokolowska.aoc.day01.Calorie
import jsokolowska.aoc.day02.RockPaperScissors
import jsokolowska.aoc.day03.Backpack
import jsokolowska.aoc.day04.CampAssignment
import jsokolowska.aoc.day05.SupplyStacks
import jsokolowska.aoc.day06.Tuning
import jsokolowska.aoc.day07.CountFiles
import jsokolowska.aoc.day08.TreeHouse
import jsokolowska.aoc.day09.RopeChase
import jsokolowska.aoc.day10.RegisterOperations
import jsokolowska.aoc.day11.KeepAway

import scala.io.Source
import scala.util.{Try, Using}

object Main {
  val fileName = "src/main/resources/day11/input.txt"

  def main(args: Array[String]): Unit = {
    val cpa = new KeepAway

    println(cpa.partTwo(readLines(fileName).get))
  }

  def readAsString(fileName: String): Try[String]
  =
    Using(Source.fromFile(fileName)) { bufferedSource =>
      bufferedSource.mkString    }

  def readLines(fileName: String): Try[List[String]]
  =
    Using(Source.fromFile(fileName)) { bufferedSource =>
      bufferedSource.getLines().toList
    }
}
package jsokolowska.aoc.day05

import scala.io.Source
import scala.util.{Try, Using}

class SupplyStacks {
  val fileName = "src/main/resources/day05/input.txt"

  def readAsString(fileName: String): Try[List[String]]
  =
    Using(Source.fromFile(fileName)) { bufferedSource =>
      bufferedSource.getLines().toList
    }


  def rearrangeCrates(): String = {
    val lines = readAsString(fileName).get
    val idx = lines.indexWhere(line => line.startsWith(" 1"))
    val stacksNo = lines(idx).split(" ").filter(_.nonEmpty).map(_.toInt).max
    var stacks = readStack(lines.take(idx), stacksNo)

    lines.slice(idx + 2, lines.length).foreach(line => {
      val ids = parseLine(line)
      stacks = move(ids._1, ids._2, ids._3, stacks)
    })

    stacks.map(_.head).mkString
  }

  def parseLine(line: String):(Int, Int, Int) = {
    val parts = line.split(" ")
    (parts(1).toInt, parts(3).toInt, parts(5).toInt)
  }

  def move(howMany:Int, from: Int, to:Int, stacks: List[List[Char]]): List[List[Char]] = {
    val elems = stacks(from-1).take(howMany).reverse
    val modifiedSourceStack = stacks(from-1).splitAt(howMany)._2
    val modifiedTargetStack = stacks(to-1).reverse.appendedAll(elems).reverse

    stacks.updated(from-1, modifiedSourceStack).updated(to-1, modifiedTargetStack)
  }

  def readStack(stacks: List[String], stackCount: Int): List[List[Char]] = {
    0.until(stackCount).map(idx => {
      val colIdxInList = idx * 4 + 1;
      stacks.filter(_.length > colIdxInList).map(_.charAt(colIdxInList)).filter(_ != ' ')
    }).toList
  }
}

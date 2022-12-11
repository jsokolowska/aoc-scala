package jsokolowska.aoc

import scala.::
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

class KeepAway {
  private enum Operation:
    case Add, Multiply, SelfMultiply, SelfAdd;
    def perform(value:Int, arg: Int): Int = {
      this match
        case Operation.Add => value + arg
        case Operation.Multiply => value * arg
        case Operation.SelfAdd => value + value
        case Operation.SelfMultiply => value * value
    }
  private class Test(var divisibleBy: Int, var monkeyTrue : Int, var monkeyFalse: Int):
    def getDestination(item: Int): Int = {
      if (item % divisibleBy == 0) monkeyTrue
      else monkeyFalse
    }

  private class Monkey(var operation: (Operation, Int),
                       var items: List[Int],
                       var test: Test) {
    var activityCounter = 0;

    def round(): List[(Int, Int)] ={
      val throwInstructions = items.map(item => {
        val worryLevelUpdated = inspect(item)
        (test.getDestination(worryLevelUpdated), worryLevelUpdated)
      })
      items = List.empty
      throwInstructions
    }

    def catchItem(item: Int) = {
      items = items.appended(item)
    }
    private def inspect(item: Int):Int = {
      activityCounter += 1
      operation._1.perform(item, operation._2) / 3
    }
  }

  def partOne(monkeyDescriptions: List[String]):Int = {
    val monkeysUnsorted = monkeyDescriptions.grouped(7).map(monkeyDesc => {
      val id = monkeyDesc.head.split(" ")(1).split(":")(0).toInt
      (id, parseMonkey(monkeyDesc.slice(1,6)))
    }).toMap
    val monkeys = ListMap(monkeysUnsorted.toSeq.sortBy(_._1):_*)

    0.until(20).foreach(* => {
      for (m <- monkeys){
        val throws = m._2.round()
        throws.foreach(t => monkeys(t._1).catchItem(t._2))
      }
    })

    val twoMostActive = monkeys.map(m => m._2.activityCounter).toList.sorted.reverse.take(2)
    twoMostActive.head * twoMostActive(1)
  }

  private def parseMonkey(monkeyDescription: List[String]): Monkey = {
    require(monkeyDescription.length == 5,
      "Can only parse 5-line descriptions")
    // parse items
    val items = monkeyDescription.head.split(": ")(1).split(", ").map(_.toInt).toList
    // parse operation

    Monkey(parseOperation(monkeyDescription(1)), items, parseTest(monkeyDescription.slice(2,5)))
  }

  private def parseOperation(operationDescription: String): (Operation, Int) = {
    val valueParts = operationDescription.split(": ")(1).split(" = ")(1).split(" ")

    // ensure no unexpected formatting
    require(valueParts.length == 3)
    require(valueParts.head == "old")

    val operator = valueParts(1)
    val rightSide = valueParts(2)
    var operatorEnum: Operation = null;
    var operationValue = 0;
    if (rightSide == "old") {
      if (operator == "*") {
        operatorEnum = Operation.SelfMultiply
      } else if (operator == "+") {
        operatorEnum = Operation.SelfAdd
      } else {
        throw new IllegalArgumentException(s"Unrecognized operator - $operator")
      }
    } else {
      if (operator == "*") {
        operatorEnum = Operation.Multiply
      } else if (operator == "+") {
        operatorEnum = Operation.Add
      } else {
        throw new IllegalArgumentException(s"Unrecognized operator - $operator")
      }
      operationValue = rightSide.toInt
    }
    (operatorEnum, operationValue)
  }

  private def parseTest(testDescription: List[String]): Test = {
    require(testDescription.length == 3)
    val div = testDescription.head.split("divisible by ")(1).toInt
    val testTrue = testDescription(1).split("true: throw to monkey ")(1).toInt
    val testFalse = testDescription(2).split("false: throw to monkey ")(1).toInt
    Test(div, testTrue, testFalse)
  }

}

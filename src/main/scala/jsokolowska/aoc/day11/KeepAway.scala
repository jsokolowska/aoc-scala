package jsokolowska.aoc.day11

import scala.::
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

class KeepAway {
  private var allPossibleDividends: Set[Int] = Set.empty
  enum Operation:
    case Add, Multiply, SelfMultiply, SelfAdd
    def perform(value:Int, arg: Int): Int = {
      this match
        case Operation.Add => value + arg
        case Operation.Multiply => value * arg
        case Operation.SelfAdd => value + value
        case Operation.SelfMultiply => value * value
    }

  abstract class Item (protected var initialValue: Int):
    protected var moduloMap: Map[Int, Int]  = Map.empty
    def isDivisibleBy(dividend: Int): Boolean = {
      moduloMap(dividend) == 0
    }
    def init (): Unit = {
      moduloMap = allPossibleDividends.map(div => (div, initialValue % div)).toMap
    }
    def inspect(operation: Operation,  argument: Int): Item

  class SimpleItem(initV: Int) extends Item(initV):
    override def inspect(operation: Operation,  argument: Int): Item = {
      //update all modulos & value
      initialValue = operation.perform(initialValue, argument) / 3
      moduloMap = moduloMap.map(mapItem => (mapItem._1, operation.perform(mapItem._2, argument) / 3))
      this
    }
    override def toString: String = initialValue.toString
  class ItemWithoutOverflow(_initialValue: Int) extends Item(_initialValue):
      override def inspect(operation: Operation, argument: Int): Item = {
        //update all modulos
        moduloMap = moduloMap.map(mapItem => (mapItem._1, operation.perform(mapItem._2, argument) % mapItem._1))
        this
      }
      override def toString: String = s"$initialValue = $moduloMap"

  private class Test(var divident: Int, var monkeyTrue : Int, var monkeyFalse: Int):
    def getDestination(item: Item): Int = {
      if (item.isDivisibleBy(divident)) monkeyTrue
      else monkeyFalse
    }

  private class Monkey(var operation: (Operation, Int),
                       var items: List[Item],
                       var test: Test) {
    var activityCounter = 0

    def round(): List[(Int, Item)] ={
      val throwInstructions = items.map(item => {
        val updatedItem = inspect(item)
        (test.getDestination(updatedItem), updatedItem)
      })
      items = List.empty
      throwInstructions
    }

    def initAllItems():Unit = {
      items.foreach(_.init())
    }

    def catchItem(item: Item): Unit = {
      items = items.appended(item)
    }
    private def inspect(item: Item):Item = {
      activityCounter += 1
      item.inspect(operation._1, operation._2)
    }
  }

  def partOne(monkeyDescriptions: List[String]):Int = {
    val monkeysUnsorted = monkeyDescriptions.grouped(7).map(monkeyDesc => {
      val id = monkeyDesc.head.split(" ")(1).split(":")(0).toInt
      (id, parseSimpleMonkey(monkeyDesc.slice(1,6)))
    }).toMap
    // init all possible dividends
    allPossibleDividends = monkeysUnsorted.map(monkey => monkey._2.test.divident).toSet
    val monkeys = ListMap(monkeysUnsorted.toSeq.sortBy(_._1):_*)
    monkeys.foreachEntry((*, m) => m.initAllItems())

    0.until(20).foreach(idx => {
      println(s"Round $idx")
      monkeys.foreach( (i, m) => println(s"$i : ${m.items}"))
      for (m <- monkeys){
        val throws = m._2.round()
        throws.foreach(t => monkeys(t._1).catchItem(t._2))
      }
    })

    val twoMostActive = monkeys.map(m => m._2.activityCounter).toList.sorted.reverse.take(2)
    twoMostActive.head * twoMostActive(1)
  }

  private def parseSimpleMonkey(monkeyDescription: List[String]): Monkey = {
    require(monkeyDescription.length == 5,
      "Can only parse 5-line descriptions")
    // parse items
    val items = monkeyDescription.head.split(": ")(1).split(", ").map(_.toInt).map(SimpleItem(_)).toList
    // parse operation

    Monkey(parseOperation(monkeyDescription(1)), items, parseTest(monkeyDescription.slice(2,5)))
  }

  private def parseMonkeyWithoutOverflow(monkeyDescription: List[String]): Monkey = {
    require(monkeyDescription.length == 5,
      "Can only parse 5-line descriptions")
    // parse items
    val items = monkeyDescription.head.split(": ")(1).split(", ").map(_.toInt).map(ItemWithoutOverflow(_)).toList
    // parse operation

    Monkey(parseOperation(monkeyDescription(1)), items, parseTest(monkeyDescription.slice(2, 5)))
  }

  private def parseOperation(operationDescription: String): (Operation, Int) = {
    val valueParts = operationDescription.split(": ")(1).split(" = ")(1).split(" ")

    // ensure no unexpected formatting
    require(valueParts.length == 3)
    require(valueParts.head == "old")

    val operator = valueParts(1)
    val rightSide = valueParts(2)
    var operatorEnum: Operation = null
    var operationValue = 0
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

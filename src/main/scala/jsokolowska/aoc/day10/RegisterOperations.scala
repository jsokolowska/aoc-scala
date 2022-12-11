package jsokolowska.aoc.day10

import scala.collection.mutable.ListBuffer

class RegisterOperations {

  private var counter = 1
  private var register = 1
  private var pendingOperations= ListBuffer[(Int, Int)]()
  private var crtPos = (0,0)
  private var drawingResult = ""

  def partOne(input: List[String]): Int = {
    input.map(_.split(" ")).map(
      instruction => {
        val inc = execute(instruction.toList)
        moveCounterAndExecutePending(inc)
      }
    ).sum
  }

  def partTwo(input: List[String]): String = {
    input.map(_.split(" ")).foreach(
      instruction => {
        val inc = execute(instruction.toList)
        moveAndDraw(inc)
      }
    )
    drawingResult
  }

  private def execute(instruction: List[String]): Int = {
    if (instruction.length == 1 & instruction.head == "noop") {
      1
    } else if (instruction.head == "addx") {
      pendingOperations.addOne((2, instruction(1).toInt))
      //println(s"$counter - $instruction")
      2
    } else {
      throw new IllegalArgumentException("Instruction not recognized")
    }
  }

  private def moveCounterAndExecutePending(increment: Int) = {
    0.until(increment).map(
      _ => {
        counter += 1
        executePending()
        registerInteresing()
      }
    ).sum
  }
  private def moveAndDraw(counterIncrement: Int) = {
    0.until(counterIncrement).foreach(
      _ => {
        draw()
        counter += 1
        moveHead()
        executePending()
      }
    )
  }
  private def moveHead() = {
    if(crtPos._2 == 39){
      crtPos = (crtPos._1 +1, 0)
      drawingResult += "\n"
    }else{
      crtPos = (crtPos._1, crtPos._2+1)
    }
  }

  private def draw() = {
    if((register - crtPos._2).abs <= 1){
      drawingResult += "#"
    }else{
      drawingResult += "."
    }
  }
  private def executePending () = {
    pendingOperations.map(op => (op._1 - 1, op._2)).filter(op => op._1 == 0).foreach(op => register += op._2)
    pendingOperations = pendingOperations.map(op => (op._1 - 1, op._2)).filter(op => op._1 > 0)
  }

  private def registerInteresing(): Int = {
    if(counter == 20 | counter == 60 | counter == 100 | counter == 140 | counter == 180 | counter == 220){
      val res = register * counter
      println(s"$counter: $register = $res")
      res
    }else{
      0
    }
  }

}

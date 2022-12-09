package jsokolowska.aoc.day09

import scala.collection.mutable

class RopeChase {

  def partOne(ropeMoves: List[String]): Int = {
    var headPos = (0, 0)
    var tailPos = (0, 0)
    val visited = mutable.Set(tailPos)

    ropeMoves.map(_.split(" ")).foreach(
      ropeMove => {
        var org = headPos
        var orgtail = tailPos
        val dir = ropeMove(0)
        val by = ropeMove(1).toInt
        val moveVector = dirToVec(dir.charAt(0))
        0.until(by).foreach(
          _ => {
            headPos = (headPos._1 + moveVector._1, headPos._2 + moveVector._2)
            if(!isAdjacent(headPos, tailPos)){
              tailPos = moveTail(headPos, tailPos)
              visited.add(tailPos)
            }
          }
        )
        //println(s"H:$org, T:$orgtail => [$dir, $by] $moveVector  => $headPos, $tailPos")
      }

    )

    visited.size
  }

  def isAdjacent(headPos: (Int, Int), tailPos: (Int, Int)): Boolean = {
    if (headPos == tailPos) return true
    if (headPos._1 == tailPos._1 & (headPos._2 - tailPos._2).abs == 1) return true
    if (headPos._2 == tailPos._2 & (headPos._1 - tailPos._1).abs == 1) return true
    if ((headPos._2 - tailPos._2).abs == 1 & (headPos._1 - tailPos._1).abs == 1) return true
    false
  }

  def dirToVec(dir: Char): (Int, Int) = {
    dir match {
      case 'R' => (0, 1)
      case 'U' => (1, 0)
      case 'D' => (-1, 0)
      case 'L' => (0, -1)
    }
  }

  def moveTail(headPos: (Int, Int), tailPos: (Int, Int)): (Int, Int) = {
    if(headPos._1 == tailPos._1){
      if(headPos._2 > tailPos._2){
        (tailPos._1, tailPos._2 + 1)
      }else{
        (tailPos._1, tailPos._2 - 1)
      }
    }else if(headPos._2 == tailPos._2){
      if (headPos._1 > tailPos._1) {
        (tailPos._1 + 1, tailPos._2)
      } else {
        (tailPos._1 - 1, tailPos._2)
      }
    }else {
      if(headPos._1 > tailPos._1 & headPos._2 > tailPos._2){
        (tailPos._1 +1, tailPos._2 + 1)
      }else if  (headPos._1 < tailPos._1 & headPos._2 > tailPos._2) {
        (tailPos._1 - 1, tailPos._2 + 1)
      } else if (headPos._1 > tailPos._1 & headPos._2 < tailPos._2) {
        (tailPos._1 + 1, tailPos._2 - 1)
      } else {
        (tailPos._1 -1, tailPos._2 - 1)
      }
    }
  }


  def partTwo(ropeMoves: List[String]): Int = {
    var headPos = (0,0)
    var pos1 = (0,0)
    var pos2 = (0,0)
    var pos3 = (0,0)
    var pos4 = (0,0)
    var pos5 = (0,0)
    var pos6 = (0,0)
    var pos7 = (0,0)
    var pos8 = (0,0)
    var pos9 = (0,0)
    var tailPos = (0,0)
    val visited = mutable.Set(tailPos)

    ropeMoves.map(_.split(" ")).foreach(
      ropeMove => {
        var org = headPos
        var orgtail = tailPos
        val dir = ropeMove(0)
        val by = ropeMove(1).toInt
        val moveVector = dirToVec(dir.charAt(0))
        0.until(by).foreach(
          _ => {
            headPos = (headPos._1 + moveVector._1, headPos._2 + moveVector._2)
            //for each next knot
            pos1 = moveIfNotAdjacent(headPos, pos1)
            pos2 = moveIfNotAdjacent(pos1, pos2)
            pos3 = moveIfNotAdjacent(pos2, pos3)
            pos4 = moveIfNotAdjacent(pos3, pos4)
            pos5 = moveIfNotAdjacent(pos4, pos5)
            pos6 = moveIfNotAdjacent(pos5, pos6)
            pos7 = moveIfNotAdjacent(pos6, pos7)
            pos8 = moveIfNotAdjacent(pos7, pos8)
            tailPos = moveIfNotAdjacent(pos8, tailPos)
            visited.add(tailPos)
          }
        )
        println(s"H:$org, T:$orgtail => [$dir, $by] $moveVector  => $headPos, $tailPos")
      }

    )

    visited.size
  }

  def moveIfNotAdjacent (posHead: (Int, Int), posNext: (Int, Int)): (Int, Int) = {
    if (!isAdjacent(posHead, posNext)) {
      moveTail(posHead, posNext)
    }else {
      posNext
    }
  }
}

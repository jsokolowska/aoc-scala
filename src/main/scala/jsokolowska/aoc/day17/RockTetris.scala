package jsokolowska.aoc.day17

import java.nio.file.WatchEvent.Kind
import scala.collection.mutable

class RockTetris {
  private val xMin = 0
  private val xMax = 7
  private val busyCoords: Map[Int, mutable.Set[Int]] = xMin.until(xMax).map((_, mutable.Set(0))).toMap
  private var jetStreams: String = ""
  private var jetIdx = 0

  enum Direction:
    case DOWN, LEFT, RIGHT

  enum Rock:
    case HLINE, PLUS, CORNER, VLINE, SQUARE


  def partOne(input: String): Int = {
    val limit = 2022
    jetStreams = input

    0.until(limit).foreach(
      idx => {
        idx % 5 match
          case 0 => fall(Rock.HLINE)
          case 1 => fall(Rock.PLUS)
          case 2 => fall(Rock.CORNER)
          case 3 => fall(Rock.VLINE)
          case 4 => fall(Rock.SQUARE)

        //printTower()
        //println("\n")
      }
    )

    maxHeight()
  }

  def printTower():Unit = {
    val yMax = maxHeight() +1
    0.to(yMax).reverse.foreach( yIdx => {
      val occY = busyCoords.filter((k,v) => v.contains(yIdx)).keySet
      val line = 0.until(xMax).map(xIdx => {
        if(occY.contains(xIdx)){
          '#'
        }else{
          '.'
        }
      }).mkString
      println(line)
    }

    )
  }

  private def fall(kind: Rock): Unit = {
    var coords: (Int, Int) = (2, maxHeight() + 4)
    coords = moveSides(kind, coords, jetStreams(jetIdx % jetStreams.length))
    jetIdx += 1

    while (canFall(kind, coords)) {
      coords = moveOne(Direction.DOWN, kind, coords)
      coords = moveSides(kind, coords, jetStreams(jetIdx % jetStreams.length))
      jetIdx += 1
    }

    stop(kind, coords)
  }

  private def moveSides(kind: Rock, coords: (Int, Int), dirChar: Char): (Int, Int) = dirChar match
    case '>' => moveOne(Direction.RIGHT, kind, coords)
    case '<' => moveOne(Direction.LEFT, kind, coords)

  private def moveOne(direction: Direction, kind: Rock, coords: (Int, Int)): (Int, Int) = {
    //println(s"\t$kind moves from $coords one to $direction")
    direction match
      case Direction.DOWN => (coords._1, coords._2 - 1)
      case Direction.RIGHT =>
        if(canMoveRight(kind, coords)){
          (coords._1 +1, coords._2)
        }else{
          coords
        }
      case Direction.LEFT =>
        if(canMoveLeft(kind, coords)){
          (coords._1 -1, coords._2)
        }else{
          coords
        }
  }

  private def canMoveRight(kind: Rock, coords: (Int, Int)): Boolean = {
    kind match
      case Rock.HLINE =>
        isFree(coords._1 + 4, coords._2)

      case Rock.VLINE =>
        isFree(coords._1 + 1, coords._2) &
          isFree(coords._1 + 1, coords._2 + 1) &
          isFree(coords._1 + 1, coords._2 + 2) &
          isFree(coords._1 + 1, coords._2 + 3)

      case Rock.SQUARE =>
        isFree(coords._1 + 2, coords._2) &
          isFree(coords._1 + 2, coords._2 + 1)

      case Rock.CORNER =>
        isFree(coords._1 + 3, coords._2) &
          isFree(coords._1 + 3, coords._2 + 1) &
          isFree(coords._1 + 3, coords._2 + 2)

      case Rock.PLUS =>
        isFree(coords._1 + 2, coords._2) &
          isFree(coords._1 + 3, coords._2 + 1) &
          isFree(coords._1 + 2, coords._2 + 2)


  }

  private def canMoveLeft(kind: Rock, coords: (Int, Int)): Boolean = {
    kind match
      case Rock.HLINE =>
        isFree(coords._1 -1, coords._2)

      case Rock.VLINE =>
        isFree(coords._1 - 1, coords._2) &
          isFree(coords._1 - 1, coords._2 + 1) &
          isFree(coords._1 - 1, coords._2 + 2) &
          isFree(coords._1 - 1, coords._2 + 3)

      case Rock.SQUARE =>
        isFree(coords._1 -1 , coords._2) &
          isFree(coords._1 -1, coords._2 + 1)


      case Rock.CORNER =>
        isFree(coords._1 -1, coords._2) &
          isFree(coords._1 +1, coords._2 + 1) &
          isFree(coords._1 + 1, coords._2 + 2)

      case Rock.PLUS =>
        isFree(coords._1, coords._2) &
          isFree(coords._1 -1, coords._2 + 1) &
          isFree(coords._1, coords._2 + 2)
  }

  private def canFall(kind: Rock, coords: (Int, Int)): Boolean = {
    kind match
      case Rock.HLINE =>
        isFree(coords._1, coords._2 - 1) &
          isFree(coords._1 + 1, coords._2 - 1) &
          isFree(coords._1 + 2, coords._2 - 1) &
          isFree(coords._1 + 3, coords._2 - 1)

      case Rock.VLINE =>
        isFree(coords._1, coords._2 - 1)

      case Rock.SQUARE =>
        isFree(coords._1, coords._2 - 1) &
        isFree(coords._1 + 1, coords._2 - 1)

      case Rock.CORNER =>
        isFree(coords._1, coords._2 - 1) &
          isFree(coords._1 + 1, coords._2 - 1) &
          isFree(coords._1 + 2, coords._2 - 1)

      case Rock.PLUS =>
        isFree(coords._1, coords._2) &
          isFree(coords._1 + 2, coords._2) &
          isFree(coords._1 + 1, coords._2 - 1)
  }

  private def isFree(xPos: Int, yPos: Int): Boolean = {
    if(xPos < xMin | xPos >= xMax) return false
    !busyCoords(xPos).contains(yPos)
  }

  private def maxHeight(): Int = busyCoords.values.flatten.max

  private def occupy(xPos: Int, yPos: Int) = busyCoords(xPos).add(yPos)

  private def stop(kind: Rock, coords: (Int, Int)): Unit = {
    kind match
      case Rock.SQUARE =>
        occupy(coords._1, coords._2)
        occupy(coords._1 + 1, coords._2)
        occupy(coords._1 + 1, coords._2 + 1)
        occupy(coords._1, coords._2 + 1)
      case Rock.HLINE =>
        occupy(coords._1, coords._2)
        occupy(coords._1 + 1, coords._2)
        occupy(coords._1 + 2, coords._2)
        occupy(coords._1 + 3, coords._2)
      case Rock.VLINE =>
        occupy(coords._1, coords._2)
        occupy(coords._1, coords._2 + 1)
        occupy(coords._1, coords._2 + 2)
        occupy(coords._1, coords._2 + 3)
      case Rock.PLUS =>
        occupy(coords._1 + 1, coords._2)
        occupy(coords._1 + 1, coords._2 + 1)
        occupy(coords._1, coords._2 + 1)
        occupy(coords._1 + 2, coords._2 + 1)
        occupy(coords._1 + 1, coords._2 + 2)
      case Rock.CORNER =>
        occupy(coords._1, coords._2)
        occupy(coords._1 + 1, coords._2)
        occupy(coords._1 + 2, coords._2)
        occupy(coords._1 + 2, coords._2 + 1)
        occupy(coords._1 + 2, coords._2 + 2)
  }

}

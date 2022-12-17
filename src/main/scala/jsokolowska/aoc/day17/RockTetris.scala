package jsokolowska.aoc.day17

import java.nio.file.WatchEvent.Kind
import scala.collection.mutable

class RockTetris {
  private val xMin: BigInt = 0
  private val xMax: BigInt = 7
  private var busyCoords: Map[BigInt, mutable.Set[BigInt]] = xMin.until(xMax).map((_, mutable.Set(BigInt.int2bigInt(0)))).toMap
  private var jetStreams: String = ""
  private var jetIdx = 0

  enum Direction:
    case DOWN, LEFT, RIGHT

  enum Rock:
    case HLINE, PLUS, CORNER, VLINE, SQUARE


  def partOne(input: String): BigInt = {
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
      }
    )

    maxHeight()
  }

  def partTwo(input: String): BigInt = {
    val limit = 1000000000000L
    var idx = 0L
    jetStreams = input

    while (idx < limit){
      if (idx % 1000 == 0) {
        prune(idx)
      }
      if (idx % 100000 == 0) {
        println(s"[${idx /100000}/${limit/100000}]")
      }
      idx % 5 match
        case 0 => fall(Rock.HLINE)
        case 1 => fall(Rock.PLUS)
        case 2 => fall(Rock.CORNER)
        case 3 => fall(Rock.VLINE)
        case 4 => fall(Rock.SQUARE)
      idx += 1

    }

    maxHeight()
  }

  def prune(idx: Long): Unit = {
    val pruneThreshold = 50
    busyCoords = busyCoords.map(mapEntry => (mapEntry._1, mapEntry._2.toList.sorted.reverse.take(pruneThreshold).to(mutable.Set) ))
  }


  private def fall(kind: Rock): Unit = {
    var coords: (BigInt, BigInt)= (2, maxHeight() + 4)
    coords = moveSides(kind, coords, jetStreams(jetIdx % jetStreams.length))
    jetIdx += 1

    while (canFall(kind, coords)) {
      coords = moveOne(Direction.DOWN, kind, coords)
      coords = moveSides(kind, coords, jetStreams(jetIdx % jetStreams.length))
      jetIdx += 1
    }

    stop(kind, coords)
  }

  private def moveSides(kind: Rock, coords: (BigInt, BigInt), dirChar: Char): (BigInt, BigInt) = dirChar match
    case '>' => moveOne(Direction.RIGHT, kind, coords)
    case '<' => moveOne(Direction.LEFT, kind, coords)

  private def moveOne(direction: Direction, kind: Rock, coords: (BigInt, BigInt)): (BigInt, BigInt) = {
    direction match
      case Direction.DOWN => (coords._1, coords._2 - 1)
      case Direction.RIGHT =>
        if (canMoveRight(kind, coords)) {
          (coords._1 + 1, coords._2)
        } else {
          coords
        }
      case Direction.LEFT =>
        if (canMoveLeft(kind, coords)) {
          (coords._1 - 1, coords._2)
        } else {
          coords
        }
  }

  private def canMoveRight(kind: Rock, coords: (BigInt, BigInt)): Boolean = {
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

  private def canMoveLeft(kind: Rock, coords: (BigInt, BigInt)): Boolean = {
    kind match
      case Rock.HLINE =>
        isFree(coords._1 - 1, coords._2)

      case Rock.VLINE =>
        isFree(coords._1 - 1, coords._2) &
          isFree(coords._1 - 1, coords._2 + 1) &
          isFree(coords._1 - 1, coords._2 + 2) &
          isFree(coords._1 - 1, coords._2 + 3)

      case Rock.SQUARE =>
        isFree(coords._1 - 1, coords._2) &
          isFree(coords._1 - 1, coords._2 + 1)


      case Rock.CORNER =>
        isFree(coords._1 - 1, coords._2) &
          isFree(coords._1 + 1, coords._2 + 1) &
          isFree(coords._1 + 1, coords._2 + 2)

      case Rock.PLUS =>
        isFree(coords._1, coords._2) &
          isFree(coords._1 - 1, coords._2 + 1) &
          isFree(coords._1, coords._2 + 2)
  }

  private def canFall(kind: Rock, coords: (BigInt, BigInt)): Boolean = {
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

  private def isFree(xPos: BigInt, yPos: BigInt): Boolean = {
    if (xPos < xMin | xPos >= xMax) return false
    !busyCoords(xPos).contains(yPos)
  }

  private def maxHeight(): BigInt = busyCoords.values.flatten.max

  private def occupy(xPos: BigInt, yPos: BigInt) = busyCoords(xPos).add(yPos)

  private def stop(kind: Rock, coords: (BigInt, BigInt)): Unit = {
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

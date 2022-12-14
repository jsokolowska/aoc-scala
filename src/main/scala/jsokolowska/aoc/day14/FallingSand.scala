package jsokolowska.aoc.day14

class FallingSand {
  private var cave: Map[Int, Set[Point]] = Map.empty
  private val ROCK = '#'
  private val SAND = 'o'
  private val SAND_SOURCE = (500, 0)
  private var floor: Int = -1

  class Point(var x: Int, var y: Int, var value: Char)

  def partOne(rockPaths: List[String]): Int = {
    rockPaths.foreach(parsePath)

    var counter = 0
    while (simulateFallingSandMolecule().nonEmpty) {
      counter += 1
    }
    //printCave(400, 600, 0, 170)
    counter
  }

  def partTwo(rockPaths: List[String]): Int = {
    rockPaths.foreach(parsePath)

    floor = cave.values.flatten.maxBy(p => p.y).y + 2

    var counter = 0
    var sandMolecule = Option(Point(0,0,SAND))
    while (sandMolecule.get.x != 500 | sandMolecule.get.y != 0) {
      sandMolecule = simulateFallingSandMolecule()
      if(sandMolecule.isEmpty){
        println(s"Something went wrong at step ${counter}")
        return counter
      }

      counter += 1
    }
    counter
  }
  def addPointToCave(point: Point) = {
    val columnList = cave.getOrElse(point.x, Set.empty) + point
    cave = cave  + (point.x -> columnList)

  }

  def simulateFallingSandMolecule(): Option[Point] = {
    val updated = fall(Point(SAND_SOURCE._1, SAND_SOURCE._2, SAND))
    if(updated.nonEmpty) addPointToCave(updated.get)
    updated
  }

  def fall(sand:Point): Option[Point] = {
    var fallingSand = Option(sand)
    while (canMove(fallingSand.get)){
      if(canFallDown(fallingSand.get)){
        fallingSand = fallDown(fallingSand.get)
      }else if(canSlideLeft(fallingSand.get)){
        fallingSand = slideLeftAndFall(fallingSand.get)
      }else if(canSlideRight(fallingSand.get)){
        fallingSand = slideRightAndFall(fallingSand.get)
      }
      if(fallingSand.isEmpty) return Option.empty
    }
    fallingSand
  }

  def canMove(sand: Point): Boolean = {
    canFallDown(sand) | canSlideLeft(sand) | canSlideRight(sand)
  }
  def canFallDown(sand:Point):Boolean = isSpaceEmpty(sand.x, sand.y + 1)
  def canSlideLeft(sand:Point):Boolean = isSpaceEmpty(sand.x - 1, sand.y + 1)
  def canSlideRight(sand:Point):Boolean = isSpaceEmpty(sand.x + 1, sand.y + 1)

  def fallDown(sand:Point): Option[Point] = {
    val blockade = getCol(sand.x).filter(p => p.y >= sand.y).sortBy(point => point.y)
    if(blockade.nonEmpty){
      val blocking = blockade.head
      Option(Point(sand.x, blocking.y - 1, sand.value))
    }else{
      //hit floor
      if(floor == -1){
        Option.empty
      }else {
        Option(Point(sand.x, floor - 1, sand.value))
      }
    }
  }

  def slideLeftAndFall(sand: Point): Option[Point] = {
    fallDown(Point(sand.x - 1, sand.y + 1, sand.value))
  }

  def slideRightAndFall(sand: Point): Option[Point] = {
    fallDown(Point(sand.x + 1, sand.y + 1, sand.value))
  }
  def isSpaceEmpty(x: Int, y: Int): Boolean = {
    !hitsTheFloor(y) & !cave.getOrElse(x, Set.empty).exists(p => p.y == y)
  }

  def hitsTheFloor(y:Int):Boolean = {
    if (floor != -1){
      y >= floor
    }else{
      false
    }
  }

  private def parsePath(path: String) = {
    val corners = path.split("->").map(corner => {
      val coords = corner.split(",").map(_.trim.toInt)
      (coords(0), coords(1))
    })
    val it = corners.iterator
    var currentPos = it.next()
    while (it.hasNext) {
      val nextPos = it.next()
      if (nextPos._1 == currentPos._1) {
        //traverse row
        val lower = List(nextPos._2, currentPos._2).min
        val upper = List(nextPos._2, currentPos._2).max

        lower.to(upper).foreach(tempPos =>
          addPointToCave(Point(nextPos._1, tempPos, ROCK))
        )
      } else {
        //traverse column
        val lower = List(nextPos._1, currentPos._1).min
        val upper = List(nextPos._1, currentPos._1).max

        lower.to(upper).foreach(tempPos =>
          addPointToCave(Point(tempPos, nextPos._2, ROCK))
        )
      }
      currentPos = nextPos
    }
  }

  private def printCave(xMin: Int, xMax: Int, yMin: Int, yMax: Int) = {
    yMin.to(yMax).foreach(rowIdx => {
      val points = getRow(rowIdx).filter(p => p.x >= xMin & p.x <= xMax)
      val rowDisplay = xMin.to(xMax).map(xValue => {
        val filt = points.filter(p => p.x == xValue)
        if (filt.nonEmpty) {
          filt.head.value
        } else {
          "."
        }
      }).mkString
      println(rowDisplay)
    })


  }

  private def getRow(row: Int): List[Point] = {
    cave.values.flatten.filter(point => point.y == row).toList
  }

  private def getCol(column: Int): List[Point] = {
    cave.getOrElse(column, Set.empty).toList
  }

}

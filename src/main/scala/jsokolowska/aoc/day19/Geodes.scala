package jsokolowska.aoc.day19

import scala.annotation.tailrec
import scala.collection.mutable

class Geodes {
  class State(private val blueprint: Blueprint):
    private val robotCounts: mutable.Map[Resource, Int] = mutable.Map(Resource.CLAY -> 0, Resource.ORE -> 1, Resource.OBSIDIAN -> 0, Resource.GEODE -> 0)
    private var robotsInManufacture: List[(Resource, Int)] = List.empty
    private val resourceCounts: mutable.Map[Resource, Int] = mutable.Map(Resource.CLAY -> 0, Resource.ORE -> 0, Resource.OBSIDIAN -> 0, Resource.GEODE -> 0)

    var operations = StringBuilder()
    private var createdFlag = false

    override def toString: String = s"${blueprint.id} - ${qualityScore()} for operations: ${operations.toString()}"

    def copy (): State = {
      val s = State(blueprint)
      s.initState(this)
      s
    }

    private def initState(other: State): Unit = {
      other.robotCounts.foreach((res, c) => this.robotCounts.addOne((res,c)))
      other.resourceCounts.foreach((res, c) => this.resourceCounts.addOne((res,c)))
      other.robotsInManufacture.foreach((robot, c) => this.robotsInManufacture = this.robotsInManufacture.appended((robot, c)))
      this.operations = StringBuilder(other.operations.toString())
    }

    def scheduleManufacture(robot: Resource): Unit = {
      createdFlag = true
      robot match
        case Resource.CLAY =>
          operations.addOne('C')
          use(Resource.ORE, blueprint.clayRobotCost)
          robotsInManufacture = robotsInManufacture.appended((robot, 1))
        case Resource.ORE =>
          operations.addOne('O')
          use(Resource.ORE, blueprint.oreRobotCost)
          robotsInManufacture = robotsInManufacture.appended((robot, 1))
        case Resource.OBSIDIAN =>
          operations.addOne('o')
          use(Resource.ORE, blueprint.obsidianRobotCost._1)
          use(Resource.CLAY, blueprint.obsidianRobotCost._2)
          robotsInManufacture = robotsInManufacture.appended((robot, 1))
        case Resource.GEODE =>
          operations.addOne('G')
          use(Resource.ORE, blueprint.geodeRobotCost._1)
          use(Resource.OBSIDIAN, blueprint.geodeRobotCost._2)
          robotsInManufacture = robotsInManufacture.appended((robot, 1))
    }

    private def use(resource: Resource, by: Int): Unit = {
      val updated = resourceCounts(resource) - by
      require(updated >= 0, "Less than 0 resources after update")
      resourceCounts += resource -> updated
    }

    def finishManufacture(): Unit = {
      robotsInManufacture.foreach(
        (robotType, count) => robotCounts += robotType -> (robotCounts(robotType) + count)
      )
      robotsInManufacture = robotsInManufacture.empty
    }

    def collect(): Unit = {
      if(!createdFlag) operations.addOne('-')
      createdFlag = false
      robotCounts.foreach(
        (robotType, count) => resourceCounts += robotType -> (count + resourceCounts(robotType))
      )
    }

    def qualityScore(): Int = {
      blueprint.id * resourceCounts(Resource.GEODE)
    }

    def enoughResourcesFor(robot: Resource): Boolean = {
      robot match
        case Resource.CLAY =>
          resourceCounts(Resource.ORE) >= blueprint.clayRobotCost
        case Resource.ORE =>
          resourceCounts(Resource.ORE) >= blueprint.oreRobotCost
        case Resource.OBSIDIAN =>
          resourceCounts(Resource.ORE) >= blueprint.obsidianRobotCost._1 &
            resourceCounts(Resource.CLAY) >= blueprint.obsidianRobotCost._2
        case Resource.GEODE =>
          resourceCounts(Resource.ORE) >= blueprint.geodeRobotCost._1 &
            resourceCounts(Resource.OBSIDIAN) >= blueprint.geodeRobotCost._2

    }

    def lessRobotsThanResourceCount(resource: Resource): Boolean = {
      if(resource == Resource.GEODE) return true
      robotCounts(resource) < maxNeededForCreation(resource)
    }

    private def maxNeededForCreation(resource: Resource):Int = {
      require(resource != Resource.GEODE)
      resource match
        case Resource.CLAY =>
          blueprint.obsidianRobotCost._2
        case Resource.ORE =>
          List(blueprint.oreRobotCost, blueprint.obsidianRobotCost._1, blueprint.geodeRobotCost._1).max
        case Resource.OBSIDIAN =>
          blueprint.geodeRobotCost._2
    }

  enum Resource:
    case CLAY, ORE, GEODE, OBSIDIAN

  class Blueprint(var id: Int, var oreRobotCost: Int, var clayRobotCost: Int,
                  var obsidianRobotCost: (Int, Int), var geodeRobotCost: (Int, Int))

  def partOne(input: List[String]): Int = {
    val blueprintRe = "Blueprint ([0-9])+: [A-Za-z ]+([0-9]+) ore.[A-Za-z ]+([0-9]+) ore. [A-Za-z ]+([0-9]+) ore [A-Za-z ]+([0-9]+) clay. [A-Za-z ]+([0-9]+) ore and ([0-9]+) obsidian.".r
    val time = 24
    input
      .flatMap(blueprintRe.findAllMatchIn(_).map(matches =>
        Blueprint(matches.group(1).toInt, matches.group(2).toInt, matches.group(3).toInt,
          (matches.group(4).toInt, matches.group(5).toInt),
          (matches.group(6).toInt, matches.group(7).toInt))))
      .map(blueprint => State(blueprint)).map(run(_, time)).map(st => {
      println(st)
      st.qualityScore()
    }).sum
  }

  private def run(state:State, time: Int): State = {
    var updatedTime = 1
    while(!state.enoughResourcesFor(Resource.CLAY)){
      state.collect()
      updatedTime += 1
    }
    state.scheduleManufacture(Resource.CLAY)
    state.collect()
    state.finishManufacture()
    depthFirstSearch(state, updatedTime+1)
  }

  private def depthFirstSearch(state: State, time: Int): State = {
    if (time == 24) {
      state.collect()
      return state
    }
    val moves = Resource.values.map( res => {
      if(state.enoughResourcesFor(res) & state.lessRobotsThanResourceCount(res)){
        Option(res)
      }else {
        Option.empty
      }
    }).appended(Option.empty).toSet


    val scores = moves.map( r => {
      val cp = state.copy()
      if(r.nonEmpty){
        cp.scheduleManufacture(r.get)
      }
      cp.collect()
      cp.finishManufacture()
      depthFirstSearch(cp, time+1)
    }).toList
    scores.sortWith((left, right) => left.qualityScore() >= right.qualityScore()).head
  }
}

package jsokolowska.aoc.day07

class CountFiles {
  class AocTree(val directory: String, var parent: AocTree) {
    var fileSize: Int = 0
    private var children: List[AocTree] = List()

    var sizeThisAndChildren = 0;

    def getChild(name: String): AocTree = {
      children.filter(_.directory == name).head
    }

    def addChild(node: AocTree): Unit = {
      children = children.appendedAll(List(node))
    }

    def countFiles(): Int = {
      if (children.isEmpty){
        sizeThisAndChildren = fileSize
      } else if (sizeThisAndChildren == 0){
        sizeThisAndChildren = children.map(_.countFiles()).sum + fileSize
      }
      sizeThisAndChildren
    }

    def getAll(): List[AocTree] = {
      if(children.isEmpty){
        return List(this)
      }
      children.flatMap(_.getAll()).appendedAll(List(this))
    }
  }

  def parseDirectoryTree(terminalLst: List[String]): Int = {
    val root: AocTree = new AocTree("/", null)
    var currentNode = root;
    var newNode: AocTree = null
    terminalLst.foreach(
      line => {
        val elems = line.split(" ")
        if (elems(0) == "$" & elems(1) == "cd") {
          currentNode = nextNode(elems(2), currentNode, root)
        } else if (elems(0) == "$" & elems(1) == "ls") {
          //do nothing
        } else if (elems(0) == "dir") {
          newNode = new AocTree(elems(1), currentNode)
          currentNode.addChild(newNode)
        } else {
          currentNode.fileSize += elems(0).toInt
        }

      })
    root.countFiles()
    val spaceNeeded = 30000000 - (70000000 - root.sizeThisAndChildren)
    root.getAll().map(_.sizeThisAndChildren).filter(_ >= spaceNeeded).min
  }

  def nextNode(dir: String, currentNode: AocTree, root: AocTree): AocTree = {
    if (dir == "..") {
      currentNode.parent
    } else if (dir == "/") {
      root
    } else {
      currentNode.getChild(dir)
    }
  }
}


package jsokolowska.aoc.day08

class TreeHouse {

  def partOne(treeRows: List[String]) = {
    val treeMap = treeRows.map(row => (row grouped 1).map(_.toInt).toList)
    val arrayName = Array.ofDim[Int](treeMap.length, treeMap.head.length)
    treeMap.indices.map(
      rowIdx => {
        treeMap(rowIdx).indices.map(
          colIdx => {
            val rowVisible = visible(colIdx, treeMap(rowIdx))
            val thisCol = treeMap.indices.map(treeMap(_)(colIdx)).toList
            val colVisible = visible(rowIdx, thisCol)
            if(rowVisible | colVisible){
              1
            }else{
              0
            }
          }
        ).sum
      }
    ).sum
  }

  def visible(idx: Int, list: List[Int]): Boolean = {
    if(idx ==0 | idx == list.length -1) {
      true
    }else{
      val parts = list.splitAt(idx)
      parts._1.max < list(idx) | parts._2.slice(1,parts._2.length).max < list(idx)
    }

  }

  def partTwo(treeRows: List[String]):Int = {
    val treeMap = treeRows.map(row => (row grouped 1).map(_.toInt).toList)
    treeMap.indices.flatMap(
      rowIdx => {
        treeMap(rowIdx).indices.map(
          colIdx => {
            val thisCol = treeMap.indices.map(treeMap(_)(colIdx)).toList
            scenicScore(rowIdx, treeMap(rowIdx), colIdx, thisCol)
          }
        )
      }
    ).max
  }

  def scenicScore(rowIdx: Int, row: List[Int], colIdx: Int, col: List[Int]): Int = {
    val thisTree = row(colIdx)
    val upScore = score(thisTree, col.slice(0,rowIdx).reverse)
    val downScore = score(thisTree, col.slice(rowIdx+1, row.length))
    val leftScore = score(thisTree, row.slice(0, colIdx).reverse)
    val rightScore = score(thisTree, row.slice(colIdx+1, col.length))
    upScore * downScore * leftScore * rightScore
  }

  def score(treeCandidate: Int, view: List[Int]): Int ={
    var counter = 0;
    var higherFound = false
    view.foreach(
      treeInView => {
        if (!higherFound & treeInView < treeCandidate){
          counter += 1
        }
        if (!higherFound & treeInView >= treeCandidate){
          counter += 1
          higherFound = true
        }
      }
    )
    counter
  }
}

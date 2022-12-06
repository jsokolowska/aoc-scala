package jsokolowska.aoc.day06

import scala.util.Try

class Tuning {
  def partOne(input: String):Int = {
    val len = input.length
    0.until(len-14).foreach(
      idx => {
        val windowStart = idx
        val windowEnd = idx+ 14
        if(allDifferent(input.slice(windowStart, windowEnd))){
          return windowEnd
        }
      }
    )
    len
  }

  def allDifferent(input: String): Boolean = {
    input.toSet.size == input.length
  }
}

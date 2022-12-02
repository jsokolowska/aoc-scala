package jsokolowska.aoc.day02

import scala.util.Try

class RockPaperScissors {

  def gradeStrategy(strategy: Try[List[String]]): Int = {
    strategy.get.map(duel =>
      outcomeScore(duel.charAt(0), duel.charAt(2)) + symbolScore(duel.charAt(2))
    ).sum
  }

  def outcomeScore(opponent: Char, me: Char): Int = {
    opponent match {
      case 'A' => //Rock
        me match {
          case 'X' => 3 //Rock
          case 'Y' => 6 //Paper
          case 'Z' => 0 //Scissors
        }
      case 'B' =>
        me match {
          case 'X' => 0
          case 'Y' => 3
          case 'Z' => 6
        }
      case 'C' =>
        me match {
          case 'X' => 6
          case 'Y' => 0
          case 'Z' => 3
        }
    }
  }

  def symbolScore(symbol: Char): Int = {
    symbol match {
      case 'X' => 1
      case 'Y' => 2
      case 'Z' => 3
    }
  }

  def chooseSymbols(strategy: Try[List[String]]): Int = {
    strategy.get.map(duel =>{
      val oponent = duel.charAt(0)
      val outcome = duel.charAt(2)
      outcome match {
        case 'X'=>  0 + chooseSymbol(oponent, outcome)
        case 'Y' => 3 + chooseSymbol(oponent, outcome)
        case 'Z'=> 6 + chooseSymbol(oponent, outcome)
      }
    }).sum
  }

  def chooseSymbol(oponent: Char, outcome: Char): Int = {
    outcome match {
      case 'X' => oponent match {
        case 'A' => 3
        case 'B' => 1
        case 'C' => 2
      }
      case 'Y' => oponent match {
        case 'A' => 1
        case 'B' => 2
        case 'C' => 3
      }
      case 'Z' => oponent match {
        case 'A' => 2
        case 'B' => 3
        case 'C' => 1
      }
    }
  }
}

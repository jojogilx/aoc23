package day2

import utils.Utils.getLines

import scala.Console.println

object Day2 {

  case class Set(r: Int, g: Int, b: Int) {
    def isPossible: Boolean = r <= 12 && g <= 13 && b <= 14

    def power: Int = r * g * b
  }



  def createSet(input: String): Set = {
    val colorPattern = """(\d+)\s(\w+)""".r

    val quantitiesAndColors = colorPattern.findAllMatchIn(input)
      .map(matched => (matched.group(1).toInt, matched.group(2)))
      .toList

    val (redCount, greenCount, blueCount) = quantitiesAndColors.foldLeft((0, 0, 0)) {
      case ((r, g, b), (quantity, color)) =>
        color.toLowerCase match {
          case "red"   => (r + quantity, g, b)
          case "green" => (r, g + quantity, b)
          case "blue"  => (r, g, b + quantity)
          case _       => (r, g, b)
        }
    }

    Set(redCount, greenCount, blueCount)
  }


  def minimumPossible(line: String): Set = {
    val sets = line.substring(line.indexOf(":")).trim.split(";") map { a =>
      createSet(a)
    }

    val (r,g,b) = sets.foldLeft((0, 0, 0)) {
      case ((r, g, b), Set(r2,g2,b2))=>
        (math.min(r2,r), math.min(g2,g), math.min(b2,b))
        }

    Set(r, g, b)
  }


  def getPowerMinimumPossibleGames(lines: List[String]): List[Int] = lines.map({
    line => {
      minimumPossible(line.replace("Game ", "")).power
    }})



  def isGamePossible(line: String): Boolean = line.substring(line.indexOf(":")).trim.split(";") forall { a=>
    createSet(a).isPossible
  }

  def getPossibleGames(lines: List[String]): List[Int] = lines.filter({
    line => {
      isGamePossible(line.replace("Game ", ""))
    }}).map(line => line.substring(0, line.indexOf(":")).replace("Game ", "").toInt)


  def main(args: Array[String]): Unit = {
    println(getPossibleGames(getLines("day2/input1.txt")).sum)
    println(getPowerMinimumPossibleGames(getLines("day2/input1.txt")).sum)
  }
}

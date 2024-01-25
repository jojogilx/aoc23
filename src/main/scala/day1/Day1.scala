package day1


import scala.util.control.NonFatal

object Day1 {

  private def getLines(file: String): List[String] = {
    val source = io.Source.fromFile(file)
    try{ source.getLines.toList}
    catch {
      case NonFatal(_) => List()
    }
    finally source.close()
  }


  private def findDigits(line : String): Int = {
    line.filter(_.isDigit) match {
      case "" => 0
      case x if x.length == 1 => (x+x).toInt
      case x => (x.head.toString+x.last.toString).toInt
    }
  }


  def sumAllLines(list: List[String], f: String => Int): Int = {
    list.map(line =>{f(line)}).sum
  }

  def digitsFromLetters(string: String): Option[Int] = {
  string match {
    case x if x.startsWith("one") => Some(1)
    case x if x.startsWith("two") => Some(2)
    case x if x.startsWith("three") => Some(3)
    case x if x.startsWith("four") => Some(4)
    case x if x.startsWith("five") => Some(5)
    case x if x.startsWith("six") => Some(6)
    case x if x.startsWith("seven") => Some(7)
    case x if x.startsWith("eight") => Some(8)
    case x if x.startsWith("nine") => Some(9)
    case _ => None
  }}

  private def findFirstAndLastDigit(line: String): Int = {
    val digits = line.zipWithIndex.collect {
      case (x, _) if x.isDigit => x.asDigit
      case (_, i) => digitsFromLetters(line.substring(i)).getOrElse(0)
    }.filter(_ != 0)

    (digits.head.toString + digits.last.toString.toInt).toInt
  }

  def main(args: Array[String]): Unit = {
    println(sumAllLines(getLines("src/main/scala/day1/input1.txt"), findDigits))
    println(sumAllLines(getLines("src/main/scala/day1/input1.txt"), findFirstAndLastDigit))
  }





}

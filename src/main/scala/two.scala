package two

import scala.util.matching.Regex
import scala.util.chaining._

@main def run() =
  val inputs = scala.io.Source.fromResource("two.txt").getLines.toList

  val re = raw"(\d+)-(\d+) (\w): (\w+)".r

  def isValid(input: String): Boolean = {

    val matches = re.findFirstMatchIn(input).get
    val min = matches.group(1).toInt
    val max = matches.group(2).toInt
    val letter = matches.group(3).charAt(0)
    val pw = matches.group(4)

    val c = pw.count(_ == letter)
    c >= min && c <= max
  }

  import scala.util.Try
  def isValidCorp(input: String): Boolean = {

    val matches = re.findFirstMatchIn(input).get
    val fst = matches.group(1).toInt
    val snd = matches.group(2).toInt
    val letter = matches.group(3).charAt(0)
    val pw = matches.group(4)

    pw.lift
    List(Try(pw(fst - 1)).toOption, Try(pw(snd - 1)).toOption)
      .flatten
      .count(_ == letter) == 1
  }

  inputs
    .map(isValid)
    .count(identity)
    .tap(println)
    
  inputs
  //   .take(5)
    .map(isValidCorp)
    .count(identity)
    .tap(println)

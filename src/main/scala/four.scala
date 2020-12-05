package four

import io.Source
import scala.collection.immutable.Vector
import scala.util.chaining._

val fields = Set(
    "byr", // (Birth Year)
    "ecl", // (Eye Color)
    "eyr", // (Expiration Year)
    "hcl", // (Hair Color)
    "hgt", // (Height)
    "iyr", // (Issue Year)
    "pid", // (Passport ID)
    // "cid", // (Country ID)
)

object validation {
    val fourDigits: String => Boolean = s => 
        s.forall(_.isDigit) && s.length == 4
    val checkInt: (Int => Boolean) => String => Boolean  = check => s => 
        s.toIntOption.map(check).getOrElse(false)
    val validYear: ( Int, Int)=> Int => Boolean =  ( min, max) => year => 
        year >= min && year <= max

    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    val validByr: String => Boolean = s => 
        fourDigits(s) &&  
        checkInt(validYear(1920,2002))(s)

    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    val validIyr: String => Boolean = s =>
        fourDigits(s) && 
        checkInt(validYear(2010,2020))(s)

    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    val validEyr: String => Boolean = s =>
        fourDigits(s) && 
        checkInt(validYear(2020,2030))(s)

    // hgt (Height) - a number followed by either cm or in:
    val validHgt: String => Boolean = s =>
        val system = s.drop(s.length-2)
        val height = s.takeWhile(_.isDigit).toIntOption.getOrElse(0)
        system match
            case "cm" => height >= 150 && height <= 193
            case "in" => height >= 59 && height <= 76
            case _    => false
    //     If cm, the number must be at least 150 and at most 193.
    //     If in, the number must be at least 59 and at most 76.

    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    val validHcl: String => Boolean = s => s.matches(raw"#[\da-f]{6}")

    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    val validEcl: String => Boolean = s => s.matches(raw"(amb|blu|brn|gry|grn|hzl|oth)")

    // pid (Passport ID) - a nine-digit number, including leading zeroes.
    val nineDigits: String => Boolean = s => 
        s.forall(_.isDigit) && s.length == 9
        
    val validPid: String => Boolean = s => s.matches(raw"\d{9}")

}

@main def run() =
    val content  = Source.fromResource("four.txt").mkString
    val entries: List[List[String]] = content.split("\n\n").map(_.replace("\n", " ")).map(_.split(" ").sorted.toList).toList

    // first(entries)
    second(entries)

def first(entries: List[List[String]]) =

    entries
        .map(
            _.map(_.split(":").head)
        )
        .count(entry => fields.forall(entry.contains))
        .tap(println)

def second(entries: List[List[String]]) =
    import validation._
  
    // cid (Country ID) - ignored, missing or not.

    entries
        .map(
            _.map{ entry =>
                val s = entry.split(":")
                (s(0), s(1))
            }
        )
        .filter(entry => fields.forall(entry.map(_._1).contains))
        .map(_.toMap)
        .count(
            map =>
                validByr(map("byr")) &&
                validEcl(map("ecl")) &&
                validEyr(map("eyr")) &&
                validHcl(map("hcl")) &&
                validHgt(map("hgt")) &&
                validIyr(map("iyr")) &&
                validPid(map("pid")) 
        )
        .tap(println)

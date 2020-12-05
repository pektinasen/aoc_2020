package five

import cats._
import cats.implicits._
import cats.effect._
import io.Source
import scala.collection.immutable.Vector
import scala.util.chaining._

@main def run() =
    val entries: List[String] = Source.fromResource("five.txt").getLines.toList

    // first(entries)
    second(entries)

def first(entries: List[String]): Unit = 
    entries
        // .take(1)
        .map { e => 
            val bString = e.map {
                case 'B' | 'R' => '1'
                case 'F' | 'L' => '0'
            }
            (Integer.parseInt(bString.take(7), 2), Integer.parseInt(bString.drop(7), 2))
        }.map((row, seat) => row * 8 + seat )
        .max
        .tap(println)


def second(entries: List[String]): Unit = 
    val occupied = entries
        .map { _.map {
                case 'B' | 'R' => '1'
                case 'F' | 'L' => '0'
            }.pipe(Integer.parseInt(_, 2))
        }.toSet
    (occupied.min to occupied.max)
        .filterNot(a => occupied.contains(a))
        .tap(println)

package three

import io.Source
import scala.collection.immutable.Vector
import scala.util.chaining._

@main def run() = 
    val lines = Source.fromResource("three.txt").getLines.toVector
    val map: Vector[Vector[Char]] = lines.map(_.toVector).transpose

    val xLen = map.length
    val yLen = map.head.length

    val slopes = List((1,1), (3,1), (5,1), (7,1), (1,2))
    val tree = '#'

    val trees = slopes.map { case (right, down) =>
        for 
            i <- 0 until yLen if (i * down) < yLen
            x = (right * i) % xLen
            y = down * i
        yield
            map(x)(y) == tree
    }

    trees
        .map(_.count(identity))
        .reduce(_ * _)
        .tap(println)

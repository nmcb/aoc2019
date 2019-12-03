package aoc

import java.io._
import scala.io.Source

object Day01 {

  val input: InputStream =
    getClass
     .getClassLoader
     .getResourceAsStream("day01.txt")

  val masses: List[Int] =
    Source
      .fromInputStream(input)
      .getLines
      .toList
      .map(_.toInt)

  assert(masses.length == 100)

  def massToFuel(m: Int): Int =
    (m / 3) - 2

  assert(massToFuel(12)     == 2)
  assert(massToFuel(14)     == 2)
  assert(massToFuel(1969)   == 654)
  assert(massToFuel(100756) == 33583)

  val result: Int =
    masses.map(massToFuel).sum
}
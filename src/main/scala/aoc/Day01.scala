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

  // Part 1

  def massToFuel1(m: Int): Int =
    (m / 3) - 2

  assert(massToFuel1(12)     == 2)
  assert(massToFuel1(14)     == 2)
  assert(massToFuel1(1969)   == 654)
  assert(massToFuel1(100756) == 33583)

  val result1: Int =
    masses.map(massToFuel1).sum

  // Part 2

  def massToFuel2(m: Int): Int = {
    val f = (m / 3) - 2
    if (f <= 0) 0 else f + massToFuel2(f)
  }

  assert(massToFuel2(12)     == 2)
  assert(massToFuel2(14)     == 2)
  assert(massToFuel2(1969)   == 966)
  assert(massToFuel2(100756) == 50346)
    
  val result2: Int =
    masses.map(massToFuel2).sum

}
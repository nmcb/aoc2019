package aoc

import java.io._
import scala.io.Source

object Day06 {

  type Name = String  

  val input: InputStream =
    getClass
      .getClassLoader
      .getResourceAsStream("day06.txt")

  // Part 1
  
  case class Planet(name: Name, center: Name)

  def parsePlanet(l: String): Planet = {
    val Array(center, name) = l.split(')').map(_.trim)
    Planet(name, center)
  }
  
  val planets: List[Planet] =
    Source
      .fromInputStream(input)
      .getLines
      .toList
      .map(parsePlanet)

  // val planets =
  //   List("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L")
  //     .map(parsePlanet)

  // val planets =
  //   List("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN")
  //     .map(parsePlanet)

  assert(planets.find(_.center == "COM").nonEmpty)

  def find(p: Planet => Boolean): Planet =
    planets.find(p).getOrElse(sys.error("boom"))

  def orbits(p: Planet, acc: Int = 1): Int =
    if (p.center == "COM")
      acc
    else
      orbits(find(_.name == p.center), acc+1)

  val result1: Int =
    planets.map(orbits(_)).sum

  // Part 2

  def pathToCom(n: Name, acc: List[Name] = Nil): List[Name] = {
    val c = find(_.name == n).center
    if (c == "COM")
      acc
    else
      pathToCom(c, c :: acc)
  }

  def path(l: List[Name] = pathToCom("YOU"), r: List[Name] = pathToCom("SAN")): List[Name] = (l, r) match {
    case (l1 :: l2 :: _ , r1 :: r2 :: _) if (l1 == r1) && (l2 == r2) => path(l.tail, r.tail)
    case (l1 :: _       , r1 :: _      ) if (l1 == r1)               => l.reverse ++ r.tail
    case _                                                           => sys.error("boom")
  }
    
  val result2 =
    path().length - 1

}
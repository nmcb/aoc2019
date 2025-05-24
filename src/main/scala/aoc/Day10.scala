package aoc

import java.io._
import scala.io._

object Day10 {

  case class Pos(x: Int, y: Int)

  def decodeStations(c: Char): Option[Int] =
    if (c == '.') None else if (c == '#') Some(0) else sys.error(s"boom: c=$c")

  def load(input: List[String]): List[Pos] = {
    val coords = input.map(_.toList.map(decodeStations))
    val maxX   = coords(0).length - 1
    val maxY   = coords.length - 1
    for {
      y <- (0 to maxY).toList
      x <- (0 to maxX).toList
      if coords(y)(x).nonEmpty
    } yield Pos(x, y)
  }
  
  val input1: InputStream =
    getClass
    .getClassLoader
    .getResourceAsStream("day10_4.txt")

  val astroids: List[Pos] =
    load(Source.fromInputStream(input1).getLines.toList)

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def blocked(a: Pos, o: Pos): Boolean = {
    val x = o.x - a.x
    val y = o.y - a.y
    val d = gcd(math.abs(x), math.abs(y))
    val nx = x / d
    val ny = y / d
    if (d > 1)
      !(1 to d - 1).forall( m => !astroids.contains(Pos(a.x + (m * nx), a.y + (m * ny))))
    else
      false
  }

  val result1 =
    astroids
      .map(astroid => astroid -> astroids.filter(_ != astroid))
      .map((astroid, others) => astroid -> others.filterNot(other => blocked(astroid, other)))
      .map((a,o) => a -> o.length)
      .sortWith((a,b) => a._2 < b._2)
      .last

  // Part 2

  val testLaserInput1: InputStream =
    getClass
    .getClassLoader
    .getResourceAsStream("day10_5.txt")

  val testLaserInput2: InputStream =
    getClass
    .getClassLoader
    .getResourceAsStream("day10_6.txt")

  val testLaserAstroids1: List[Pos] =
    load(Source.fromInputStream(testLaserInput1).getLines.toList)

  val testLaserAstroids2: List[Pos] =
    load(Source.fromInputStream(testLaserInput2).getLines.toList)
      
  def distance(a: Pos, o: Pos): Double =
    val x = o.x - a.x
    val y = o.y - a.y
    math.sqrt(math.pow(x.toDouble, 2) + math.pow(y.toDouble, 2))

  assert( distance(Pos(0, 0), Pos(3, 4)) == 5.0)

  def angle(a: Pos, o: Pos): Double = {
    val x = o.x - a.x
    val y = o.y - a.y
    val d = 90 - math.atan2(-y.toDouble, x.toDouble) * 180 / math.Pi
    if (d >= 0) d else d + 360
  }

  assert( angle(Pos(200, 200), Pos(200, 0  )) ==   0.0 ) 
  assert( angle(Pos(200, 200), Pos(400, 0  )) ==  45.0 ) 
  assert( angle(Pos(200, 200), Pos(400, 200)) ==  90.0 ) 
  assert( angle(Pos(200, 200), Pos(400, 400)) == 135.0 ) 
  assert( angle(Pos(200, 200), Pos(200, 400)) == 180.0 ) 
  assert( angle(Pos(200, 200), Pos(0  , 400)) == 225.0 ) 
  assert( angle(Pos(200, 200), Pos(0  , 200)) == 270.0 )
  assert( angle(Pos(200, 200), Pos(0  , 0  )) == 315.0 ) 

  import Ordering.Double.TotalOrdering

  def testLaser(a: Pos)(as: List[Pos]): List[(Double, Double, Pos)] =
    as
      .filterNot(_ == a)
      .map(o => (angle(a, o), distance(a, o), o))
      .sortBy(s => (s._1, s._2, s._3.x, s._3.y))

  @scala.annotation.tailrec
  def fireAll(nr: Int)(as: List[(Double, Double, Pos)], acc: List[(Double, Double, Pos)] = Nil, count: Int = 1, res: Option[Pos] = None): Option[Pos] =
    if (as.isEmpty && acc.nonEmpty) fireAll(nr)(acc, Nil, count, res)
    else if (as.isEmpty) res
    else {
      val h = as.head
      val t = as.tail
      val l = t.takeWhile(a => a._1 == h._1)
      val r = t.dropWhile(a => a._1 == h._1)
      println(s"[$count] pos=${h._3}")
      fireAll(nr)(r, acc ++ l, count + 1, if (count == nr) Some(h._3) else res)
    }

  assert ( fireAll( 36)(testLaser(Pos(8, 3))(testLaserAstroids1)) == Some(Pos(14,3)) )
  assert ( fireAll(200)(testLaser(Pos(11,13))(testLaserAstroids2)) == Some(Pos(8,2)) )

  val result2 = 
    fireAll(200)(testLaser(Pos(8,16))(astroids))
}
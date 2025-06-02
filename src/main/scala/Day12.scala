import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Vec(x: Int, y: Int, z: Int):

    infix def +(that: Vec): Vec = copy(x = x + that.x, y = y + that.y, z = z + that.z)
    infix def -(that: Vec): Vec = copy(x = x - that.x, y = y - that.y, z = z - that.z)

    def sign: Vec = Vec(x.sign, y.sign, z.sign)
    def manhattan: Int = x.abs + y.abs + z.abs

  object Vec:
    val zero: Vec = Vec(0,0,0)

  case class Moon(position: Vec, velocity: Vec):
    def energy: Int =
      position.manhattan * velocity.manhattan

  val moons: Vector[Moon] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"<x=$x, y=$y, z=$z>" => Moon(Vec(x.toInt, y.toInt, z.toInt), Vec.zero)
      .toVector

  extension (moons: Vector[Moon])
    def step: Vector[Moon] =
      moons.map: moon =>
        val velocity = moons.foldLeft(moon.velocity): (result,other) =>
          result + (other.position - moon.position).sign
        Moon(moon.position + velocity, velocity)

  import Iterator.*

  val start1  = System.currentTimeMillis
  val answer1 = iterate(moons)(_.step).drop(1000).next.map(_.energy).sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = 666
  println(s"Day $day answer part 2:\n$answer2 [${System.currentTimeMillis - start2}ms]")

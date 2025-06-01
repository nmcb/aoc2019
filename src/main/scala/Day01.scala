import scala.annotation.tailrec
import scala.io.Source

object Day01 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val masses: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toInt)
      .toVector

  def massToFuel(mass: Int): Int =
    (mass / 3) - 2

  val start1  = System.currentTimeMillis
  val answer1 = masses.map(massToFuel).sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def massToFuelRequirement(mass: Int): Int =
    @tailrec
    def go(mass: Int, total: Int = 0): Int =
      val requirement = massToFuel(mass)
      if requirement <= 0 then
        total
      else
        go(requirement - 1, total + requirement)
    go(mass)

  val start2  = System.currentTimeMillis
  val answer2 = masses.map(massToFuelRequirement).sum
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")

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

  def massToFuel1(mass: Int): Int =
    (mass / 3) - 2

  val start1  = System.currentTimeMillis
  val answer1 = masses.map(massToFuel1).sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def massToFuel2(mass: Int): Int =
    @tailrec
    def go(mass: Int, result: Int = 0): Int =
      val f = (mass / 3) - 2
      if f <= 0 then result else go(f - 1, result + f)
    go(mass)

  val start2 = System.currentTimeMillis
  val answer2 = masses.map(massToFuel2).sum
  println(s"Day $day answer part 1: $answer2 [${System.currentTimeMillis - start1}ms]")

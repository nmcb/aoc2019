import scala.annotation.tailrec
import scala.io.Source

object Day19 extends App:

  import cpu.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  def pulls(program: Mem, xs: Range, ys: Range): IndexedSeq[Value] =
    for
      y <- ys
      x <- xs
    yield
      CPU(program).withInput(x,y).outputs.last


  val program = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  val start1  = System.currentTimeMillis
  val answer1 = pulls(program, 0 until 50, 0 until 50).count(_ == 1)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def wiggleSouthEast(program: Mem, maxX: Value, maxY: Value): Long =

    def isPulled(x: Value, y: Value): Boolean =
      CPU(program).withInput(x,y).outputs.head == 1

    @tailrec
    def go(x: Value, y: Value): Value =
      val ne = isPulled(x, y - maxY)
      val sw = isPulled(x - maxX, y)

      if ne && sw then
        10000 * (x - maxX) + (y - maxY)
      else if ne then
        go(x + 1, y)
      else
        go(x, y + 1)

    go(maxX, maxY)

  val start2  = System.currentTimeMillis
  val answer2 = wiggleSouthEast(program, maxX = 99, maxY = 99)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

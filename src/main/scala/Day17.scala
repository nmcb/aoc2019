import scala.io.Source

object Day17 extends App:

  import cpu.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val program: Mem =
    Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  case class Pos(x: Int, y: Int):
    def +(that: Pos): Pos = copy(x = x + that.x, y = y + that.y)

  val neighbours = Seq(Pos(0,-1), Pos(0,1), Pos(-1,0), Pos(1,0))

  def solve1(memory: Mem): Int =
    val output = CPU(memory).outputs.map(_.toChar).mkString.split("\n")
    val points =
      for y <- 0 until output.length
          x <- 0 until output(0).length
          if output(y)(x) == '#'
      yield
        Pos(x, y)

    output.foreach(println)

    points.foldLeft(0): (total,next) =>
      if neighbours.map(next + _).forall(points.contains) then
        total + (next.x * next.y)
      else total

  val start1  = System.currentTimeMillis
  val answer1 = solve1(program)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  
  val start2  = System.currentTimeMillis
  val answer2 = 666
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

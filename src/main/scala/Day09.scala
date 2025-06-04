import scala.io.Source

object Day09 extends App:

  import cpu.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val program: Mem = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  val start1  = System.currentTimeMillis
  val answer1 = CPU(program).withInput(1).outputs.last
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")
  
  val start2  = System.currentTimeMillis
  val answer2 = CPU(program).withInput(2).outputs.last
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

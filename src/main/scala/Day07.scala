import scala.annotation.tailrec
import scala.io.Source

object Day07 extends App:

  import cpu.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString
  val program = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  def highestSignal(program: Mem, phaseSettings: Range): Value =
    phaseSettings
      .permutations
      .map(_.foldLeft(0L): (total,setting) =>
        CPU(program).withInput(setting,total).outputs.last)
      .max

  val start1  = System.currentTimeMillis
  val answer1 = highestSignal(program, 0 to 4)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  
  def highestSignalWithFeedback(program: Mem, phaseSettings: Range): Value =

    def signalWithFeedback(settings: Vector[Value]): Value =
      lazy val outputs: LazyList[Value] =
        settings.foldLeft(0L #:: outputs): (inputs, setting) =>
          CPU(program, setting #:: inputs).outputs

      outputs.last

    phaseSettings
      .map(_.toLong)
      .toVector
      .permutations
      .map(signalWithFeedback)
      .max
  
  val start2  = System.currentTimeMillis
  val answer2 = highestSignalWithFeedback(program, 5 to 9)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

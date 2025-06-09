import scala.io.Source

object Day21 extends App:

  import cpu.*

  val day     = getClass.getSimpleName.filter(_.isDigit).mkString
  val program = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  /** ((A | -(B & C)) & D) */
  val walk =
    Seq(
      "OR A J",
      "AND B J",
      "AND C J",
      "NOT J J",
      "AND D J"
    )

  /** (-A | -C | -B) & D & (E | H) */
  val run =
    Seq(
      "NOT A J",
      "NOT C T",
      "OR T J",
      "NOT B T",
      "OR T J",
      "AND D J",
      "NOT D T",
      "OR E T",
      "OR H T",
      "AND T J"
    )

  def survey(program: Mem, script: Seq[String]): Long =
    val input = script.map(_ + "\n").mkString.map(_.toLong)
    CPU(program).withInput(input *).outputs.last

  val start1  = System.currentTimeMillis
  val answer1 = survey(program, script = walk :+ "WALK")
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = survey(program, script = run :+ "RUN")
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

import scala.io.Source

object Day02 extends App:

  import cpu.*

  val day     = getClass.getSimpleName.filter(_.isDigit).mkString
  val program = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  val start1 = System.currentTimeMillis
  val answer1 = CPU(program.updated(1,12).updated(2,2)).execFinal.mem(0)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def experiment(program: Mem): Int =
    val permutations =
      for
        noun <- 1 to 99
        verb <- 1 to 99
      yield
        (noun, verb)

    val (noun, verb) =
      permutations
        .dropWhile: (noun,verb) =>
          val patch = program.updated(1,noun).updated(2,verb)
          CPU(patch).execFinal.mem(0) != 19690720
        .head

    100 * noun + verb

  val start2 = System.currentTimeMillis
  val answer2 = experiment(program)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

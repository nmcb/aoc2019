import scala.annotation.tailrec
import scala.io.Source

object Day02 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val program: Vector[Int] =
    Source.fromResource(s"input$day.txt").mkString.trim.split(",").map(_.toInt).toVector


  def interpret(program: Vector[Int]): Int =
    @tailrec
    def go(ip: Int, code: Seq[Int]): Int =
      code(ip) match
        case 1 => go(ip + 4, code.updated(code(ip + 3), code(code(ip + 1)) + code(code(ip + 2))))
        case 2 => go(ip + 4, code.updated(code(ip + 3), code(code(ip + 1)) * code(code(ip + 2))))
        case 99 => code.head

    go(0, program)

  val start1 = System.currentTimeMillis
  val answer1 = interpret(program.updated(1, 12).updated(2, 2))
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def experiment(input: Vector[Int]): Int =
    val permutations =
      for
        noun <- 1 to 99
        verb <- 1 to 99
      yield
        (noun, verb)

    val (noun, verb) =
      permutations
        .dropWhile: (noun,verb) =>
          interpret(input.updated(1, noun).updated(2, verb)) != 19690720
        .head
    100 * noun + verb

  val start2 = System.currentTimeMillis
  val answer2 = experiment(program)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")

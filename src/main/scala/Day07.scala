import Day07.State.Running

import scala.annotation.tailrec
import scala.io.Source

object Day07 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val Div = Map(1 -> 100, 2 -> 1000)

  type Mem   = Vector[Int]
  type Input = Vector[Int]

  extension (mem: Mem)
    def get(ip: Int, offset: Int): Int =
      mem(ip) / Div(offset) % 10 match
        case 0 => mem(mem(ip + offset))
        case 1 => mem(ip + offset)

  enum State:
    case Initial
    case Running
    case Halted
    case Output(output: Int)

  import State.*

  case class CPU(mem: Mem, input: Input = Vector.empty, ip: Int = 0, state: State = Initial):

    private def binop(op: (Int,Int) => Int): CPU =
      copy(
        mem   = mem.updated(mem(ip + 3), op(mem.get(ip, 1), mem.get(ip, 2))),
        ip    = ip + 4,
        state = Running
      )

    private def ingress(): CPU =
      copy(
        mem   = mem.updated(mem(ip + 1), input.head),
        input = input.tail,
        ip    = ip + 2,
        state = Running
      )

    private def exgress(): CPU =
      copy(
        ip    = ip + 2,
        state = Output(output = mem.get(ip, 1))
      )

    private def branch(condition: Int => Boolean): CPU =
      copy(
        ip    = if condition(mem.get(ip, 1)) then mem.get(ip, 2) else ip + 3,
        state = Running
      )

    private def when(condition: (Int,Int) => Boolean): CPU =
      copy(
        mem   = mem.updated(mem(ip + 3), if condition(mem.get(ip, 1), mem.get(ip, 2)) then 1 else 0),
        ip    = ip + 4,
        state = Running
      )

    private def halt(): CPU =
      copy(
        state = Halted
      )

    private def step: CPU =
      mem(ip) % 100 match
        case 1  => binop(_ + _)
        case 2  => binop(_ * _)
        case 3  => ingress()
        case 4  => exgress()
        case 5  => branch(_ != 0)
        case 6  => branch(_ == 0)
        case 7  => when(_  < _)
        case 8  => when(_ == _)
        case 99 => halt()

    def withInput(next: Int*): CPU =
      copy(input = input :++ next)

    def run =
      Iterator
        .iterate(step)(_.step)
        .dropWhile(_.state == Running)
        .next

    def allOutput: Vector[Int] =
      Iterator
        .iterate(this)(_.run)
        .takeWhile(_.state != Halted)
        .map(_.state)
        .collect:
          case Output(value) => value
        .toVector

  val program: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(",")
      .map(_.toInt)
      .toVector

  def highestSignal(program: Mem): Int =
    (0 to 4)
      .permutations
      .map: phaseSettings =>
        phaseSettings
          .foldLeft(0): (total,next) =>
            CPU(program)
              .withInput(next,total)
              .allOutput
              .last
      .max

  val start1  = System.currentTimeMillis
  val answer1 = highestSignal(program)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def highestSignalWithFeedback(code: Mem): Int =
    @tailrec
    def runner(input: Int)(amplifiers: Seq[CPU]): Int =
      val amplifier = amplifiers.head.withInput(input).run
      amplifier.state match
        case Output(output) => runner(output)(amplifiers.tail :+ amplifier)
        case Halted         => input
        case _              => sys.error(s"illegal state=${amplifier.state}")

    (5 to 9)
      .map: phaseSettings =>
        CPU(code).withInput(phaseSettings)
      .permutations
      .map(runner(0))
      .max

  val start2  = System.currentTimeMillis
  val answer2 = highestSignalWithFeedback(program)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")

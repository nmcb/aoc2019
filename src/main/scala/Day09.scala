import scala.annotation.tailrec
import scala.io.Source

object Day09 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val Div = Map(1 -> 100, 2 -> 1000, 3 -> 10000)

  type Mem   = Map[Long,Long]
  type Input = Vector[Long]

  enum State:
    case Initial
    case Running
    case Halted
    case Output(output: Long)

  import State.*

  case class CPU(mem: Mem, base: Long = 0, input: Input = Vector.empty, ip: Long = 0, state: State = Initial):

    extension (mem: Mem)

      def read(offset: Int): Long =
        mem(ip) / Div(offset) % 10 match
          case 0 => mem(mem(ip + offset))
          case 1 => mem(ip + offset)
          case 2 => mem(base + mem(ip + offset))

      def write(offset: Int, value: Long): Mem =
        mem(ip) / Div(offset) % 10 match
          case 0 => mem.updated(mem(ip + offset), value)
          case 2 => mem.updated(base + mem(ip + offset), value)

    private def binop(op: (Long,Long) => Long): CPU =
      copy(
        mem   = mem.write(3, op(mem.read(1), mem.read(2))),
        ip    = ip + 4,
        state = Running
      )

    private def ingress(): CPU =
      copy(
        mem   = mem.write(1, input.head),
        input = input.tail,
        ip    = ip + 2,
        state = Running
      )

    private def exgress(): CPU =
      copy(
        ip    = ip + 2,
        state = Output(output = mem.read(1))
      )

    private def branch(condition: Long => Boolean): CPU =
      copy(
        ip    = if condition(mem.read(1)) then mem.read(2) else ip + 3,
        state = Running
      )

    private def when(condition: (Long,Long) => Boolean): CPU =
      copy(
        mem   = mem.write(3, if condition(mem.read(1), mem.read(2)) then 1 else 0),
        ip    = ip + 4,
        state = Running
      )

    private def rebase(): CPU =
      copy(
        base  = base + mem.read(1),
        ip    = ip + 2,
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
        case 9  => rebase()
        case 99 => halt()

    def withInput(next: Long*): CPU =
      copy(input = input :++ next)

    def run =
      Iterator
        .iterate(step)(_.step)
        .dropWhile(_.state == Running)
        .next

    def allOutput: Vector[Long] =
      Iterator
        .iterate(this)(_.run)
        .takeWhile(_.state != Halted)
        .map(_.state)
        .collect:
          case Output(value) => value
        .toVector

  object CPU:
    def load(input: Vector[Long]): CPU =
      CPU(
        mem =
          input
            .zipWithIndex
            .map: (value,index) =>
              index.toLong -> value
            .toMap
            .withDefaultValue(0)
      )

  val program: Vector[Long] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(",")
      .map(_.toLong)
      .toVector

  val start1  = System.currentTimeMillis
  val answer1 = CPU.load(program).withInput(1).allOutput.last
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")
  
  val start2  = System.currentTimeMillis
  val answer2 = CPU.load(program).withInput(2).allOutput.last
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val Div = Map(1 -> 100, 2 -> 1000)

  type Mem = Vector[Int]
  type IO  = Vector[Int]

  extension (mem: Mem)
    def get(ip: Int, offset: Int): Int =
      mem(ip) / Div(offset) % 10 match
        case 0 => mem(mem(ip + offset))
        case 1 => mem(ip + offset)

  case class CPU(mem: Mem, in: IO, ip: Int = 0, out: IO = Vector.empty):

    private def binop(op: (Int,Int) => Int): CPU =
      copy(mem = mem.updated(mem(ip+3), op(mem.get(ip,1), mem.get(ip,2))), ip = ip+4)

    private def ingress(): CPU =
      copy(mem = mem.updated(mem(ip+1), in.head), in = in.tail, ip = ip+2)

    private def exgress(): CPU =
      copy(ip = ip+2, out = out :+ mem.get(ip,1))

    private def branch(condition: Int => Boolean): CPU =
      copy(ip = if condition(mem.get(ip,1)) then mem.get(ip,2) else ip+3)

    private def when(condition: (Int,Int) => Boolean): CPU =
      copy(mem = mem.updated(mem(ip+3), if condition(mem.get(ip,1), mem.get(ip,2)) then 1 else 0), ip = ip+4)

    @tailrec
    final def run: CPU =
      mem(ip) % 100 match
        case 1  => binop(_ + _).run
        case 2  => binop(_ * _).run
        case 3  => ingress().run
        case 4  => exgress().run
        case 5  => branch(_ != 0).run
        case 6  => branch(_ == 0).run
        case 7  => when(_  < _).run
        case 8  => when(_ == _).run
        case 99 => this

  val program: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(",")
      .map(_.toInt)
      .toVector

  val start1  = System.currentTimeMillis
  val answer1 = CPU(mem = program, in = Vector(1)).run.out.last
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis
  val answer2 = CPU(mem = program, in = Vector(5)).run.out.last
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

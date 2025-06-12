import scala.annotation.tailrec
import scala.io.Source

object Day25 extends App:

  import cpu.*

  def day: String  = getClass.getSimpleName.filter(_.isDigit).mkString
  def program: Mem = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  def description(cpu: CPU): CPU =
    @tailrec
    def go(cpu: CPU, message: String): CPU =
      if message.endsWith("Command?\n") || message.endsWith("!\n\n") || message.endsWith("\"\n") then
        print(message)
        cpu
      else
        cpu.executeOne match
          case Some(next, Some(output)) => go(next, message :+ output.toChar)
          case Some(next, None)         => go(next, message)
          case None                     => sys.error(s"unexpected halt")
    go(cpu, "")

  def solve1(cpu: CPU, recorded: Seq[String]): Unit =
    @tailrec
    def go(computer: CPU, command: String, recorded: Seq[String]): Unit =
      println(command)
      if command == "exit" then
        ()
      else
        val input = command.map(_.toLong) :+ 10L
        val next  = description(computer.withInput(input *))

        val (fetched, record) =
          recorded match
            case first +: tail => (first, tail)
            case _             => (Console.in.readLine(), Seq())

        go(next, fetched, record)

    go(CPU(program), "", recorded)

  val solution = "d2VzdAp3ZXN0CnRha2UgYm93bCBvZiByaWNlCmVhc3QKbm9ydGgKZWFzdApzb3V0aAp0YWtlIGRhcmsgbWF0dGVyCm5vcnRoCndlc3QKbm9ydGgKdGFrZSBjYW5keSBjYW5lCndlc3QKd2VzdApub3J0aAp0YWtlIGRlaHlkcmF0ZWQgd2F0ZXIKd2VzdApzb3V0aA=="
  val recorded = Seq.empty // String(java.util.Base64.getDecoder.decode(solution)).split("\n").toSeq

  val start1  = System.currentTimeMillis
  val answer1 = solve1(CPU(program), recorded)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

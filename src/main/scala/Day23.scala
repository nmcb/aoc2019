import scala.annotation.tailrec
import scala.io.Source

object Day23 extends App:

  import cpu.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class State(cpu: CPU, in: Seq[Value], out: Seq[Value])

  def step(network: Seq[State], nat: Seq[Value]): (Seq[State], Seq[Value]) =
    val next =
      network.map:
        case State(cpu, in, out) =>
          val run =
            if cpu.stdin.nonEmpty then
              cpu.executeOne
            else if in.nonEmpty then
              cpu.withInput(in *).executeOne
            else
              cpu.withInput(-1L).executeOne

          val nextInput = if cpu.stdin.nonEmpty then in else Seq.empty

          run match
            case None                   => sys.error(s"unexpected halt")
            case Some(cpu, None)        => State(cpu, nextInput, out)
            case Some(cpu, Some(value)) => State(cpu, nextInput, out :+ value)

    next.zipWithIndex.foldLeft((next, nat)):
      case ((network, nat), (state, source)) =>
        if state.out.length != 3 then
          (network, nat)
        else
          val dest = state.out.head.toInt
          val from = state.copy(out = Seq())
          if dest == 255 then
            (network.updated(source, from), state.out.tail)
          else
            val to = network(dest).copy(in = network(dest).in ++ state.out.tail)
            (network.updated(source, from).updated(dest, to), nat)

  def solve1(memory: Mem): Value =
    @tailrec
    def go(network: Seq[State], nat: Seq[Value]): Value =
      step(network, nat) match
        case (_, Seq(_, registerY)) => registerY
        case (nextNetwork, nextNat) => go(nextNetwork, nextNat)

    val network = Seq.tabulate(50)(i => State(CPU(memory), Seq(i), Seq.empty))
    go(network, Seq.empty)

  val program = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  val start1  = System.currentTimeMillis
  val answer1 = solve1(program)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def solve2(memory: Mem): Value =
    @tailrec
    def go(network: Seq[State], nat: Seq[Value], count: Int, lastY: Option[Value]): Value =
      val (nextNetwork, nextNat, nextCount, nextLastY) =
        if count < 700 then
          val (nextNetwork, nextNat) = step(network, nat)
          val active = network.exists(state => state.in.nonEmpty || state.out.nonEmpty)
          val nextCount = if active then 0 else count + 1
          (nextNetwork, nextNat, nextCount, None)
        else
          val nextNetwork = network.updated(0, network(0).copy(in = nat))
          (nextNetwork, nat, 0, Some(nat.last))

      (lastY, nextLastY) match
        case (Some(previous), Some(next)) if previous == next =>
          next
        case _ =>
          go(nextNetwork, nextNat, nextCount, nextLastY.orElse(lastY))

    val network = Seq.tabulate(50)(i => State(CPU(memory), Seq(i), Seq.empty))
    go(network, Seq.empty, 0, None)


  val start2  = System.currentTimeMillis
  val answer2 = solve2(program)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

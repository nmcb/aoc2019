import scala.collection.*
import scala.io.Source

object Day15 extends App:

  import cpu.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    def +(that: Pos): Pos = copy(x = x + that.x, y = y + that.y)

  val neighbours: Vector[(Pos,Int)] =
    Vector(Pos(0, -1) -> 1, Pos(0, 1) -> 2, Pos(-1, 0) -> 3, Pos(1, 0) -> 4)

  case class Tile(pos: Pos, cpu: CPU, status: Long)

  def move(pos: Pos, cpu: CPU)(delta: Pos, command: Int): Tile =
    val (next, status) = cpu.withInput(command).outputStates.last
    Tile(pos + delta, next, status)

  def dijkstra(cpu: CPU): (Map[Pos,Int], Option[(Pos,CPU)]) =
    val steps = mutable.Map(Pos(0,0) -> 0)
    val todo  = mutable.PriorityQueue(Pos(0,0) -> cpu)(using Ordering.by((point, _) => steps(point)))
    var target = Option.empty[(Pos, CPU)]

    while todo.nonEmpty do
      val (from, cpu) = todo.dequeue
      neighbours
        .map(move(from, cpu))
        .filter(tile => !steps.contains(tile.pos) || steps(from) + 1 < steps(tile.pos))
        .filter(_.status > 0)
        .foreach: tile =>
          steps(tile.pos) = steps(from) + 1
          todo.enqueue(tile.pos -> cpu)
          if tile.status == 2 then target = Some(tile.pos -> cpu)

    (steps.toMap, target)

  def solve1(program: Mem): Int =
    val (first, Some(target, _)) = dijkstra(CPU(program)): @unchecked
    first(target)

  val program: Mem = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  val start1  = System.currentTimeMillis
  val answer1 = solve1(program)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  
  def solve2(memory: Mem): Int =
    val (_, Some(_, computer)) = dijkstra(CPU(memory)): @unchecked
    val (second, _) = dijkstra(computer)
    second.values.max

  val start2  = System.currentTimeMillis
  val answer2 = solve2(program)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

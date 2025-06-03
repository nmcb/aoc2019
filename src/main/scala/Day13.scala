import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  import cpu.*

  enum Tile(val id: Long):
    case Empty  extends Tile(0)
    case Wall   extends Tile(1)
    case Block  extends Tile(2)
    case Paddle extends Tile(3)
    case Ball   extends Tile(4)

  object Tile:
    def fromId(id: Long): Tile =
      Tile.values.find(_.id == id).get

  import Tile.*

  case class Pos(x: Long, y: Long)

  extension (outputs: LazyList[Value])
    def render: Map[Pos,Tile] =
      outputs.grouped(3).foldLeft(Map.empty[Pos,Tile]):
        case (result, LazyList(x,y,id)) => result + (Pos(x,y) -> Tile.fromId(id))
        case output                     => sys.error(s"invalid output: $output")

  val program = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)
  val start1  = System.currentTimeMillis
  val answer1 = CPU(program).outputs.render.values.count(_ == Block)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  type Pixel = (Pos, Tile)

  extension (pixel: Pixel)
    def pos: Pos   = pixel._1
    def tile: Tile = pixel._2

  case class GameState(paddle: Option[Pos], ball: Option[Pos], score: Option[Value], display: Map[Pos,Tile]):

    def updated(pos: Pos, value: Value): GameState =
      (pos,value) match
        case (Pos(-1, 0), score) => copy(score   = Some(score))
        case (pos, 3)            => copy(paddle  = Some(pos), display = display.updated(pos, Paddle))
        case (pos, 4)            => copy(ball    = Some(pos), display = display.updated(pos, Ball))
        case (pos, tile)         => copy(display = display.updated(pos, Tile.fromId(tile)))

    def echo(): Unit =
      val Home   = "\u001b[H"
      val Clear  = "\u001b[2J"
      val Bold   = "\u001b[1m"
      val Reset  = "\u001b[0m"
      val Green  = "\u001b[32m"
      val Yellow = "\u001b[33m"
      val Red    = "\u001b[31m"
      val Blue   = "\u001b[34m"
      val White  = "\u001b[97m"

      val sprites =
        Map(
          Empty  -> (               " "        ),
          Wall   -> (Green +        "█"        ),
          Block  -> (Blue  +        "░"        ),
          Paddle -> (White + Bold + "‒" + Reset),
          Ball   -> (Red   + Bold + "O" + Reset),
        )

      val buffer = collection.mutable.ListBuffer(Home + Clear)
      buffer += s"$White$Bold  Score: ${score.getOrElse(666)}  Blocks: ${display.values.count(_ == Block)} $Reset\n"

      val maxX = display.maxBy(_.pos.x).pos.x.toInt
      val maxY = display.maxBy(_.pos.y).pos.y.toInt

      for y <- 0 to maxY do
        for x <- 0 to maxX do
          buffer += sprites(display(Pos(x,y)))
        buffer += "\n"
      buffer += Reset

      println(buffer.mkString)
      Thread.sleep(5)


  def play(program: Mem, onScreen: Boolean = false): Value =
    @tailrec
    def go(cpu: CPU, game: GameState): Value =
      val executions = cpu.executeAll
      if executions.isEmpty then
        game.score.get
      else
        val outputs = executions.flatMap(_.outputOption)
        val frame   = outputs.grouped(3).foldLeft(game):
          case (result, LazyList(x,y,value)) => result.updated(Pos(x,y), value)
          case (_, _)                        => sys.error("incomplete output")

        val ballX   = frame.ball.get.x
        val paddleX = frame.paddle.get.x
        val input   = ballX.compareTo(paddleX).sign
        val next    = executions.last.cpu.copy(stdin = LazyList(input))

        if onScreen then frame.echo()
        go(next, frame)

    go(CPU(program + (0 -> 2L)), GameState(None, None, None, Map.empty.withDefaultValue(Empty)))

  val start2  = System.currentTimeMillis
  val answer2 = play(program)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")


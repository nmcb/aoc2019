import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Area(x: Int, y: Int, amount: Int):
    def delta(dx: Int, dy: Int): Area =
      copy(x = x + dx, y = y + dy)

    def axis: Seq[Area] =
      Seq((1, 0), (-1, 0), (0, 1), (0, -1)).map(delta)

  def neighbouring2D(area: Area): Seq[Area] =
    area.axis.filter(area => area.x >= 0 && area.x < 5 && area.y >= 0 && area.y < 5)

  def neighbouring3D(area: Area): Seq[Area] =
    area.axis.flatMap:
      case Area(-1, y, amount) => Set(Area(1, 2, amount - 1))
      case Area(5, y, amount)  => Set(Area(3, 2, amount - 1))
      case Area(x, -1, amount) => Set(Area(2, 1, amount - 1))
      case Area(x, 5, amount)  => Set(Area(2, 3, amount - 1))
      case Area(2, 2, amount)  => area match
        case Area(2, 1, amount) => Set.tabulate(5)(x => Area(x, 0, amount + 1))
        case Area(2, 3, amount) => Set.tabulate(5)(x => Area(x, 4, amount + 1))
        case Area(1, 2, amount) => Set.tabulate(5)(y => Area(0, y, amount + 1))
        case Area(3, 2, amount) => Set.tabulate(5)(y => Area(4, y, amount + 1))
        case _                  => sys.error(s"illegal area: $area")
      case other => Set(other)

  def step(grid: Set[Area], neighbours: Area => Seq[Area]): Set[Area] =
    val candidates = grid ++ grid.flatMap(neighbours)
    candidates.flatMap: area =>
      (grid.contains(area), neighbours(area).count(grid.contains)) match
        case (_, 1)     => Some(area)
        case (false, 2) => Some(area)
        case _          => None

  def solve1(eris: Set[Area]): Int =
    @tailrec
    def go(eris: Set[Area], previous: Set[Set[Area]]): Int =
      val next = step(eris, neighbouring2D)
      if previous.contains(next) then
        next.map(area => 1 << (area.x + 5 * area.y)).sum
      else
        go(next, previous + next)

    go(eris, Set())

  val eris: Set[Area] =
    val lines = Source.fromResource(s"input$day.txt").getLines.toSeq
    Set.tabulate(5, 5)((x, y) => Option.when(lines(y)(x) == '#')(Area(x, y, 0))).flatten.flatten

  import Iterator.*

  val start1  = System.currentTimeMillis
  val answer1 = solve1(eris)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  val start2  = System.currentTimeMillis
  val answer2 = iterate(eris)(step(_, neighbouring3D)).drop(200).next.size
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

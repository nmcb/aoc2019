import scala.io.Source

object Day03 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val Vector(description1, description2) =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toVector

  case class Pos(x: Int, y: Int):

    def manhattan: Int = math.abs(x) + math.abs(y)

  object Pos:
    def origin: Pos =
      Pos(0, 0)

    given Ordering[Pos] =
      Ordering.fromLessThan((a,b) => a.manhattan < b.manhattan)

  def append(wire: Vector[Pos], cmd: String): Vector[Pos] =

    val length: Int = cmd.drop(1).toInt
    val origin: Pos = wire.lastOption.getOrElse(Pos.origin)

    cmd.head match
      case 'R' => wire ++ (1 to length).map(i => Pos(origin.x + i, origin.y))
      case 'L' => wire ++ (1 to length).map(i => Pos(origin.x - i, origin.y))
      case 'U' => wire ++ (1 to length).map(i => Pos(origin.x, origin.y + i))
      case 'D' => wire ++ (1 to length).map(i => Pos(origin.x, origin.y - i))

  def wire(description: String): Vector[Pos] =
    description.split(',').foldLeft(Vector.empty)((wire,cmd) => append(wire, cmd))

  def distances(description1: String, description2: String): Vector[Pos] =
    wire(description1).toSet.intersect(wire(description2).toSet).toVector.sorted

  val start1 = System.currentTimeMillis
  val answer1 = distances(description1, description2).head.manhattan
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  type Crossing = (Pos,Int)

  extension (crossing: Crossing)
    def pos: Pos   = crossing._1
    def count: Int = crossing._2

  def minCrossingsOnIntersection(crossings1: Vector[Crossing], crossings2: Vector[Crossing]): Crossing =
    val intersections = crossings1.map(_.pos).toSet.intersect(crossings2.map(_.pos).toSet).toVector
    val crossingCount1 = crossings1.filter(crossing => intersections.contains(crossing.pos)).toMap
    val crossingCount2 = crossings2.filter(crossing => intersections.contains(crossing.pos)).toMap

    intersections
      .map(pos => pos -> (crossingCount1(pos) + crossingCount2(pos) + 2))
      .sortWith(_.count < _.count)
      .head

  def solve2(description1: String, description2: String): Int =
    val crossings1 = wire(description1).zipWithIndex
    val crossings2 = wire(description2).zipWithIndex
    minCrossingsOnIntersection(crossings1,crossings2).count

  val start2 = System.currentTimeMillis
  val answer2 = solve2(description1, description2)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")

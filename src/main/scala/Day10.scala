import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):

    def distance(o: Pos): Double =
      val dx = o.x - x
      val dy = o.y - y
      math.sqrt(math.pow(dx.toDouble, 2) + math.pow(dy.toDouble, 2))

    def angle(o: Pos): Double =
      val dx = (o.x - x).toDouble
      val dy = (o.y - y).toDouble
      val d = 90 - math.atan2(-dy, dx) * 180 / math.Pi
      if d >= 0 then d else d + 360


    def part2: Int =
      x * 100 + y


  val astroids: List[Pos] =
    val coords = Source.fromResource(s"input$day.txt").getLines.toVector
    val sizeX = coords(0).length
    val sizeY = coords.length
    List
      .tabulate(sizeX, sizeY)((x,y) => if coords(y)(x) == '#' then Some(Pos(x,y)) else None)
      .flatten
      .flatten

  def blockedBy(astroids: List[Pos], a: Pos, o: Pos): Boolean =

    @tailrec
    def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    val x = o.x - a.x
    val y = o.y - a.y
    val d = gcd(math.abs(x), math.abs(y))
    val nx = x / d
    val ny = y / d
    (1 until d).exists(m => astroids.contains(Pos(a.x + (m * nx), a.y + (m * ny))))

  def maxBlockedByCount(astroids: List[Pos]): Int =

      def blockCount(astroid: Pos): Int =
        astroids
          .filterNot(other => other != astroid && blockedBy(astroids, astroid,other))
          .length

      astroids
        .groupMapReduce(identity)(blockCount)(_ max _)
        .values
        .max - 1

  val start1  = System.currentTimeMillis
  val answer1 = maxBlockedByCount(astroids)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  type Aim = (Double,Double,Pos)

  extension (aim: Aim)
    def angle: Double    = aim._1
    def distance: Double = aim._2
    def target: Pos      = aim._3

  def testLaser(astroid: Pos)(astroids: List[Pos]): List[Aim] =
    import Ordering.Double.TotalOrdering
    astroids
      .filterNot(_ == astroid)
      .map(other => (astroid.angle(other), astroid.distance(other), other))
      .sortBy(s => (s.angle, s.distance, s.target.x, s.target.y))

  @tailrec
  def fireAll(nr: Int)(todo: List[Aim], done: List[Aim] = List.empty, count: Int = 1, result: Option[Pos] = None): Option[Pos] =
    if todo.isEmpty && done.nonEmpty then
      fireAll(nr)(done, List.empty, count, result)
    else if todo.isEmpty then
      result
    else
      val aim   = todo.head
      val rest  = todo.tail
      val left  = rest.takeWhile(a => a.angle == aim.angle)
      val right = rest.dropWhile(a => a.angle == aim.angle)
      fireAll(nr)(right, done ++ left, count + 1, Option.when (count == nr)(aim.target).orElse(result))

  val start2  = System.currentTimeMillis
  val answer2 = fireAll(200)(testLaser(Pos(8,16))(astroids)).get.part2
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

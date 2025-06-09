import scala.io.Source

object Day18 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    def neighbours: Seq[Pos] =
      def delta(dx: Int, dy: Int): Pos = copy(x = x + dx, y = y + dy)
      Seq(delta(0,-1), delta(0,1), delta(-1,0), delta(1,0))

  case class Move(from: Pos, to: Pos, cost: Int, keysNeeded: Set[Char])

  extension (c: Char)
    def isRobot: Boolean = c == '@'
    def isWall: Boolean  = c == '#'
    def isKey: Boolean   = 'a' <= c && c <= 'z'
    def isDoor: Boolean  = 'A' <= c && c <= 'Z'

  def dijkstra(tunnels: Map[Pos,Char], start: Pos): Vector[Move] =
    val cost = collection.mutable.Map(start -> 0)
    val keys = collection.mutable.Map(start -> Set.empty[Char])
    val todo = collection.mutable.PriorityQueue(start)(using Ordering.by(cost))

    while todo.nonEmpty do
      val pos = todo.dequeue
      pos.neighbours.view
        .filter(next => !tunnels(next).isWall)
        .filter(next => !cost.contains(next) || cost(pos) + 1 < cost(next))
        .foreach: next =>
          cost(next) = cost(pos) + 1
          keys(next) = if tunnels(next).isDoor then keys(pos) + tunnels(next).toLower else keys(pos)
          todo.enqueue(next)

    cost.keys.map(pos => Move(start, pos, cost(pos), keys(pos))).toVector

  type Tile = (Pos,Char)

  extension (tile: Tile)
    def pos: Pos     = tile._1
    def symbol: Char = tile._2

  def explore(tunnels: Map[Pos,Char], keys: Set[Pos], robots: Set[Pos]): Int =

    val routes = (keys ++ robots)
      .flatMap(pos => dijkstra(tunnels, pos))
      .map(move => (move.from, move.to) -> move)
      .toMap

    val cache = collection.mutable.Map.empty[(Set[Pos],Set[Char]), Int]
    def go(todo: Set[Pos], robots: Set[Pos], found: Set[Char], total: Int, result: Int): Int =
      if total >= result || total >= cache.getOrElse((robots,found), Int.MaxValue) then
        result
      else if todo.isEmpty then
        total
      else
        cache((robots,found)) = total
        val candidates = for from <- robots; to <- todo yield routes.get((from,to))
        candidates
          .toVector
          .flatten
          .filter(_.keysNeeded.subsetOf(found))
          .sortBy(_.cost)
          .foldLeft(result):
            case (result, Move(from, to, cost, _)) =>
              go(
                todo   = todo - to,
                robots = robots - from + to,
                found  = found + tunnels(to),
                total  = total + cost,
                result = result
              )

    go(
      todo   = keys,
      robots = robots,
      found  = Set.empty,
      total  = 0,
      result = Int.MaxValue
    )

  def parse(lines: Vector[String]): (tunnels: Map[Pos,Char], keys: Set[Pos], robots: Set[Pos]) =
    val tunnels = for y <- lines.indices; x <- lines(0).indices yield Pos(x, y) -> lines(y)(x)
    val keys    = tunnels.filter(_.symbol.isKey).map(_.pos)
    val robots  = tunnels.filter(_.symbol.isRobot).map(_.pos)
    (tunnels.toMap, keys.toSet, robots.toSet)


  val lines = Source.fromResource(s"input$day.txt").getLines.toVector
  val (tunnels, keys, robots) = parse(lines)

  val start1  = System.currentTimeMillis
  val answer1 = explore(tunnels, keys, robots)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def solve2(lines: Vector[String]): Int =
    val patched = lines
      .updated(39, lines(39).patch(39, "@#@", 3))
      .updated(40, lines(40).patch(39, "###", 3))
      .updated(41, lines(41).patch(39, "@#@", 3))
    val (tunnels, keys, robots) = parse(patched)
    explore(tunnels, keys, robots)

  val start2  = System.currentTimeMillis
  val answer2 = solve2(lines)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

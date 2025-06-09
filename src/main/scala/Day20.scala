import scala.io.Source

object Day20 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Pos = copy(x = x + dx, y = y + dy)

  case class State(path: Seq[(Portal,Int)], total: Int)

  enum Portal:
    case Start
    case End
    case Outer(label: String)
    case Inner(label: String)

  import Portal.*

  def portalToPortalRoutes(lines: Seq[String]): Map[Portal,Set[(Portal,Int)]] =
    def peek(x: Int, y: Int): Char = lines.lift(y).flatMap(_.lift(x)).getOrElse(' ')
    val sizeX   = lines.maxBy(_.length).length
    val sizeY   = lines.length
    val maze    = Seq.tabulate(sizeX, sizeY)((x,y) => Pos(x,y) -> peek(x,y)).flatten.toMap
    val portals = findPortals(sizeX - 3, sizeY - 3, maze)

    val routes =
      for
        (start,portal) <- portals
      yield
        val steps      = bfs(start, maze)
        val candidates = steps.keySet.intersect(portals.keySet) - start
        portal -> candidates.map(point => portals(point) -> steps(point))

    routes.map: (portal,routes) =>
      val linkedRoutes = routes.map:
        case (Outer(label),steps) => (Inner(label), steps + 1)
        case (Inner(label),steps) => (Outer(label), steps + 1)
        case other                => other
      portal -> linkedRoutes

  def findPortals(sizeX: Int, sizeY: Int, maze: Map[Pos,Char]): Map[Pos,Portal] =
    val patterns =
      Seq(Seq((-2,0), (-1,0)), Seq((1,0), (2,0)), Seq((0,-2), (0,-1)), Seq((0,1), (0,2)))

    val portals = for
      x       <- 2 to sizeX
      y       <- 2 to sizeY
      pattern <- patterns
    yield
      val pos = Pos(x,y)
      val Seq(first,second) = pattern.map(pos.delta).map(maze)
      if maze(pos) == '.' && first.isLetter && second.isLetter then
        val label = s"$first$second"
        val key =
          if label == "AA" then
            Start
          else if label == "ZZ" then
            End
          else if x == 2 || y == 2 || x == sizeX || y == sizeY then
            Outer(label)
          else
            Inner(label)
        Some(pos -> key)
      else
        None
    portals.flatten.toMap

  def bfs(start: Pos, maze: Map[Pos,Char]): Map[Pos,Int] =
    val cost = collection.mutable.Map(start -> 0)
    val todo = collection.mutable.Queue(start)
    val neighbours = Seq((-1,0), (1,0), (0,-1), (0,1))

    while todo.nonEmpty do
      val point = todo.dequeue
      neighbours
        .map(point.delta)
        .filter(next => maze(next) == '.')
        .filter(next => cost(point) + 1 < cost.getOrElse(next, Int.MaxValue))
        .foreach: next =>
          cost(next) = cost(point) + 1
          todo.enqueue(next)

    cost.toMap

  def explore(lines: Seq[String], recursive: Boolean): Int =
    val routes = portalToPortalRoutes(lines)
    val todo   = collection.mutable.Queue(State(Seq(Start -> 0), 0))
    var result = Int.MaxValue

    while todo.nonEmpty do
      val State(path, total) = todo.dequeue
      val (current, depth)   = path.last

      if total >= result then
        ()
      else if current == End then
        result = total
      else
        routes(current)
          .filter: (portal, steps) =>
            if !recursive then
              true
            else
              portal match
                case Inner(_) if depth == 0   => false
                case Start | End if depth > 0 => false
                case _                        => true
          .foreach: (portal, steps) =>
            val next = portal match
              case outer: Outer => (outer, depth + 1)
              case inner: Inner => (inner, depth - 1)
              case other => (other, depth)

            if !path.contains(next) then
              todo.enqueue(State(path.appended(next), total + steps))

    result


  val lines = Source.fromResource(s"input$day.txt").getLines.toSeq

  val start1  = System.currentTimeMillis
  val answer1 = explore(lines, recursive = false)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = explore(lines, recursive = true)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

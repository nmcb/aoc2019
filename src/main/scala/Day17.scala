import scala.io.Source

object Day17 extends App:

  import cpu.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val program: Mem =
    Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  case class Pos(x: Int, y: Int):
    def left: Pos = Pos(y, -x)
    def right: Pos = Pos(-y, x)

    def +(that: Pos): Pos = copy(x = x + that.x, y = y + that.y)

  val neighbours = Seq(Pos(0,-1), Pos(0,1), Pos(-1,0), Pos(1,0))

  def solve1(memory: Mem): Int =
    val output = CPU(memory).outputs.map(_.toChar).mkString.split("\n")
    val points =
      for y <- 0 until output.length
          x <- 0 until output(0).length
          if output(y)(x) == '#'
      yield
        Pos(x, y)

    points.foldLeft(0): (total,next) =>
      if neighbours.map(next + _).forall(points.contains) then
        total + (next.x * next.y)
      else total

  val start1  = System.currentTimeMillis
  val answer1 = solve1(program)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** @see Credits https://github.com/sim642/adventofcode */

  enum Move(override val toString: String):
    case Left            extends Move("L")
    case Right           extends Move("R")
    case Forward(steps: Int) extends Move(steps.toString)
    
  import Move.*   

  type Path = List[Move]
  type Grid = Vector[Vector[Char]]

  def pathToString(path: Path): String = path.mkString(",")

  extension (grid: Grid)

    def findRobot: (Pos,Pos) =
      def parseRobotDirection(tile: Char): Option[Pos] =
        tile match
          case '>' => Some(Pos(1,0))
          case '<' => Some(Pos(-1,0))
          case 'v' => Some(Pos(0,1))
          case '^' => Some(Pos(0,-1))
          case _   => None

      val robots =
        for
          (row, y)  <- grid.view.zipWithIndex
          (tile, x) <- row.view.zipWithIndex
          direction <- parseRobotDirection(tile)
        yield
          (Pos(x,y), direction)

      robots.head

    def getPath: Path =

      def getTile(pos: Pos): Char =
        if (0 <= pos.x && 0 <= pos.y && pos.y < grid.size && pos.x < grid(pos.y).size)
          grid(pos.y)(pos.x)
        else
          '.'

      def go(pos: Pos, direction: Pos): Path =
        if getTile(pos + direction) == '#' then
          go(pos + direction, direction) match
            case Forward(n) :: tl => Forward(n + 1) :: tl
            case tl => Forward(1) :: tl
        else if getTile(pos + direction.left) == '#' then
          Left :: go(pos, direction.left)
        else if getTile(pos + direction.right) == '#' then
          Right :: go(pos, direction.right)
        else
          Nil

      val (pos, direction) = grid.findRobot
      go(pos, direction)


  def split[A](seq: List[A], delimiter: List[A]): List[List[A]] =
    val i = seq.indexOfSlice(delimiter)
    if i < 0 then
      List(seq)
    else
      val prefix = seq.take(i)
      val suffix = seq.drop(i + delimiter.size)
      prefix +: split(suffix, delimiter)

  private val pathStringMaxLength: Int = 20
  private val pathMaxLength: Int = (20 + 1) / 2

  def factorPathParts(pathParts: Seq[Path], maxParts: Int = 3): Iterator[List[Path]] =
    if pathParts.isEmpty then
      Iterator(Nil)
    else if maxParts <= 0 then
      Iterator.empty
    else
      val firstPathPart = pathParts.head
      for
        n <- (1 to (firstPathPart.size min pathMaxLength)).reverseIterator
        init = firstPathPart.take(n)
        if pathToString(init).lengthIs <= pathStringMaxLength
        newPathParts = pathParts.flatMap(split(_, init).filter(_.nonEmpty))
        tailPathParts <- factorPathParts(newPathParts, maxParts - 1)
      yield
        init :: tailPathParts

  def reconstructMainPaths(path: Path, pathParts: Seq[Path]): Iterator[List[Int]] =
    if path.isEmpty then
      Iterator(Nil)
    else
      for
        (pathPart, i) <- pathParts.iterator.zipWithIndex
        if path.startsWith(pathPart)
        tailPath = path.drop(pathPart.size)
        tailMainPath  <- reconstructMainPaths(tailPath, pathParts)
      yield
        i :: tailMainPath

  def mainPathToString(mainPath: Seq[Int]): String =
    mainPath.map(i => ('A' + i).toChar).mkString(",")

  def dustCollected(program: Mem, grid: Grid): Int =
    val path      = getPath(grid)
    val pathParts = factorPathParts(Seq(path)).next
    val mainPath  = reconstructMainPaths(path, pathParts).next

    val newProgram      = program + (0 -> 2L)
    val mainPathString  = mainPathToString(mainPath)
    val pathPartsString = pathParts.map(pathToString).mkString("\n")
    val inputString =
      s"""$mainPathString
         |$pathPartsString
         |n
         |""".stripMargin
    val inputs = inputString.map(_.toLong).to(LazyList)

    CPU(newProgram, stdin = inputs.to(LazyList)).outputs.last.toInt

  def dustCollected(program: Mem): Int =
    dustCollected(program, parseInputGrid(program))

  def parseGrid(input: String): Grid =
    input.linesIterator.map(_.toVector).toVector

  def parseInputGrid(program: Mem): Grid =
    parseGrid(CPU(program).outputs.map(_.toChar).mkString)

  val start2  = System.currentTimeMillis
  val answer2 = dustCollected(program)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

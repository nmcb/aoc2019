import scala.annotation.tailrec
import scala.io.Source

object Day06 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Name = String

  case class Planet(name: Name, center: Name)

  val planets: List[Planet] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"$center)$name" => Planet(name, center)
      .toList

  def find(p: Planet => Boolean): Planet =
    planets.find(p).getOrElse(sys.error("boom"))
  
  def orbits(p: Planet): List[Name] =
    @tailrec
    def go(p: Planet, acc: List[Name] = List.empty): List[Name] =
      if (p.center == "COM")
        p.name :: acc
      else
        val next = find(_.name == p.center)
        go(next, next.name :: acc)

    go(p)

  val start1  = System.currentTimeMillis
  val answer1 = planets.map(orbits).map(_.length).sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def pathToCom(n: Name): List[Name] =
    @tailrec
    def go(n: Name, acc: List[Name] = List.empty): List[Name] =
      val c = find(_.name == n).center
      if (c == "COM")
        acc
      else
        go(c, c :: acc)
    go(n)

  def path(l: List[Name], r: List[Name]): List[Name] =
    (l,r) match
      case (l1 :: l2 :: _ , r1 :: r2 :: _) if l1 == r1 && l2 == r2 => path(l.tail, r.tail)
      case (l1 :: _       , r1 :: _      ) if l1 == r1             => l.reverse ++ r.tail
      case _                                                       => sys.error(s"unmatched l=$l, r=$r")

  val start2  = System.currentTimeMillis
  val answer2 = path(pathToCom("YOU"), pathToCom("SAN")).length
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")

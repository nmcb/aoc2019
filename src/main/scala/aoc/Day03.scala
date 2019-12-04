package aoc

object Day03 {

  import java.io.InputStream
  import scala.io.Source

  val input: InputStream =
    getClass
     .getClassLoader
     .getResourceAsStream("day03.txt")

  val Seq(desc1, desc2) =
    Source
      .fromInputStream(input)
      .getLines.toSeq

  case class Pos(val x: Int, val y: Int) {
    def distance: Int =
      math.abs(x) + math.abs(y)
    override def toString: String =
      s"Pos(x=$x,y=$y,d=$distance)"
  }
  object Pos {
    def origin: Pos =
      Pos(0, 0)
    implicit val posOrdering: Ordering[Pos] = new Ordering[Pos] {
      override def compare(a: Pos, b: Pos): Int = {
        a.distance.compareTo(b.distance)
      }
    }
  }

  def append(wire: Seq[Pos], cmd: String): Seq[Pos] = {
    val len: Int = cmd.drop(1).toInt
    val org: Pos = wire.lastOption.getOrElse(Pos.origin)
    cmd.head match {
      case 'R' => wire ++ (1 to len).map(i => Pos(org.x + i, org.y))
      case 'L' => wire ++ (1 to len).map(i => Pos(org.x - i, org.y))
      case 'U' => wire ++ (1 to len).map(i => Pos(org.x, org.y + i))
      case 'D' => wire ++ (1 to len).map(i => Pos(org.x, org.y - i))
    }
  }

  def wire(desc: String): Seq[Pos] =
    desc.split(',').foldLeft(Seq.empty)((wire, cmd) => append(wire, cmd))

  assert(wire("U1,R2,D3,L4") == Seq(
    Pos(0,1),Pos(1,1),Pos(2,1),Pos(2,0),Pos(2,-1),Pos(2,-2),Pos(1,-2),Pos(0,-2),Pos(-1,-2),Pos(-2,-2))
  )

  def distances(w1: String, w2: String): Seq[Pos] =
    wire(w1).toSet.intersect(wire(w2).toSet).toSeq.sortWith((p1, p2) => p1.distance < p2.distance)

  assert(distances("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83")
    .head.distance == 159)
  assert(distances("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    .head.distance == 135)
    
  val result1: Pos =
    distances(desc1, desc2).head

  // Part 2

  def minStepOnPosIntersection(w1: Seq[(Pos, Int)], w2: Seq[(Pos, Int)]): (Pos, Int) = {
    val intersections = w1.map(_._1).toSet.intersect(w2.map(_._1).toSet).toSeq
    val w1s = w1.filter(x => intersections.contains(x._1)).map(x => x._1 -> x._2).toMap
    val w2s = w2.filter(x => intersections.contains(x._1)).map(x => x._1 -> x._2).toMap

    intersections
      .map(p => p -> (w1s.getOrElse(p, sys.error("boom")) + w2s.getOrElse(p, sys.error("boom")) + 2))
      .sortWith((t1, t2) => t1._2 < t2._2)
      .head
  }

  assert { 
    val w1 = wire("R75,D30,R83,U83,L12,D49,R71,U7,L72").zipWithIndex
    val w2 = wire("U62,R66,U55,R34,D71,R55,D58,R83").zipWithIndex
    minStepOnPosIntersection(w1, w2)._2 == 610
  }

  assert { 
    val w1 = wire("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51").zipWithIndex
    val w2 = wire("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7").zipWithIndex
    minStepOnPosIntersection(w1, w2)._2 == 410
  }

  val result2: String = {
    val (pos, min) = minStepOnPosIntersection(wire(desc1).zipWithIndex, wire(desc2).zipWithIndex)
    s"$pos -> $min"
  }     
    
}
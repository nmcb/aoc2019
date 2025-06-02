import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Vec(x: Int, y: Int, z: Int):

    infix def +(that: Vec): Vec = copy(x = x + that.x, y = y + that.y, z = z + that.z)
    infix def -(that: Vec): Vec = copy(x = x - that.x, y = y - that.y, z = z - that.z)

    def sign: Vec = Vec(x.sign, y.sign, z.sign)
    def manhattan: Int = x.abs + y.abs + z.abs

  object Vec:
    val zero: Vec = Vec(0,0,0)

  case class Moon(position: Vec, velocity: Vec):
    def energy: Int =
      position.manhattan * velocity.manhattan

  val moons: Vector[Moon] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"<x=$x, y=$y, z=$z>" => Moon(Vec(x.toInt, y.toInt, z.toInt), Vec.zero)
      .toVector

  extension (moons: Vector[Moon])

    def step: Vector[Moon] =
      moons.map: moon =>
        val velocity = moons.foldLeft(moon.velocity): (result,other) =>
          result + (other.position - moon.position).sign
        Moon(moon.position + velocity, velocity)

  import Iterator.*

  val start1  = System.currentTimeMillis
  val answer1 = iterate(moons)(_.step).drop(1000).next.map(_.energy).sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /**
   * The key insights are:  (1) The axes (x,y,z) are totally independent so it suffices to find
   * the period for each axis separately and the answer is the lcm of these.  (2) Each axis will
   * repeat "relatively quickly" (e.g. fast enough to brute force).  And (3) since each state
   * has a unique parent, the first repeat must be a repeat of state 0.
   */

  extension (a: Long)

    @tailrec
    infix def gcd(b: Long): Long =
      if a % b == 0 then b else b gcd (a % b)

    infix def lcm(b: Long): Long =
      a * b / (a gcd b)

  extension (moons: Vector[Moon])

    private def periodOf(dimension: Vec => Int): Long =

      def same(vs: Vector[(Vec,Vec)]): Boolean =
        vs.forall((a,b) => dimension(a) == dimension(b))

      @tailrec
      def go(next: Vector[Moon], count: Long = 1): Long =
        val step    = next.step
        val samePos = same(step.zip(moons).map((a,b) => (a.position, b.position)))
        val sameVel = same(step.zip(moons).map((a,b) => (a.velocity, b.velocity)))
        if samePos && sameVel then count else go(next = step, count = count + 1)
      go(moons)

    def period: Long =
      val periodX = periodOf(_.x)
      val periodY = periodOf(_.y)
      val periodZ = periodOf(_.z)
      periodX lcm periodY lcm periodZ

  val start2  = System.currentTimeMillis
  val answer2 = moons.period
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

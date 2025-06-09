import scala.io.Source

object Day22 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  /**
   * I have no idea why this works...
   *
   * @see Credits - https://github.com/maneatingape
   */

  case class Technique(a: BigInt, c: BigInt, m: BigInt):
    def mod(n: BigInt) =
      n % m

    def shuffle(index: Long): Long =
      mod(a * index + c).toLong

    infix def merge(that: Technique): Technique =
      val nextA = mod(a * that.a)
      val nextC = mod(c * that.a + that.c)
      copy(a = nextA, c = nextC)

    def inverse: Technique =
      val nextA = a.modInverse(m)
      val nextC = mod(nextA * -c)
      copy(a = nextA, c = nextC)

    def pow(exp: BigInt): Technique =
      val nextA = a.modPow(exp, m)
      val nextC = mod((nextA - 1) * (a - 1).modInverse(m) * c)
      copy(a = nextA, c = nextC)

  def solve(input: Seq[String], size: Long): Technique =
    input.map(_.split(" "))
      .map:
        case Array(_, "into", _, _) => Technique(size - 1, size - 1, size)
        case Array(_, "with", _, n) => Technique(n.toLong, 0, size)
        case Array("cut", n)        => Technique(1, size - n.toLong, size)
      .reduce(_ merge _)

  def solve1(input: Seq[String]): Long =
    solve(input, 10007).shuffle(2019)

  val rules: Seq[String] =
    Source.fromResource(s"input$day.txt").getLines.toSeq

  val start1  = System.currentTimeMillis
  val answer1 = solve1(rules)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(input: Seq[String]): Long =
    solve(input, 119315717514047L).inverse.pow(101741582076661L).shuffle(2020)

  val start2  = System.currentTimeMillis
  val answer2 = solve2(rules)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

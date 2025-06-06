import scala.io.Source

object Day16 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  import Iterator.*

  def solve1(signal: Seq[Int]): String =
    def phase(signal: Seq[Int]): Seq[Int] =
      signal.indices.map: digit =>
        val raw = signal.zipWithIndex.map: (next,index) =>
          ((index + 1) / (digit + 1)) % 4 match
            case 0 | 2 => 0
            case 1     => next
            case 3     => -next
        (raw.sum % 10).abs
    iterate(signal)(phase).drop(100).next.take(8).mkString

  val signal: Seq[Int] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .map(_.asDigit)
      .toVector

  val start1  = System.currentTimeMillis
  val answer1 = solve1(signal)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  
  def solve2(signal: String): String =
    def phase(signal: Seq[Int]): Seq[Int] =
      signal.scanRight(0)(_ + _).map(_ % 10)

    val index   = signal.take(7).mkString.toInt
    val initial = signal.drop(index).map(_.asDigit)
    iterate(initial)(phase).drop(100).next.take(8).mkString

  val start2  = System.currentTimeMillis
  val answer2 = solve2(signal.mkString * 10000)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

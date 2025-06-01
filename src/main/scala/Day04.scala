object Day04 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  def solve1(first: Int, last: Int): Int =
    (first to last).count: number =>
      def window = number.toString.map(_.asDigit).sliding(2)

      val rule1 = window.forall { case Seq(a,b) => a <= b }
      val rule2 = window.exists { case Seq(a,b) => a == b }
      rule1 && rule2

  val start1 = System.currentTimeMillis
  val answer1 = solve1(235741, 706948)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def solve2(first: Int, last: Int): Int =
    (first to last).count: number =>
      def window(size: Int) = number.toString.map(_.asDigit).sliding(size)

      val rule1 = window(2).forall  { case Seq(a,b)                       => a <= b }
      val rule2 = window(2).collect { case Seq(a,b)   if a == b           => a }.toSet
      val rule3 = window(3).collect { case Seq(a,b,c) if a == b && b == c => a }.toSet
      rule1 && (rule2 -- rule3).nonEmpty

  val start2 = System.currentTimeMillis
  val answer2 = solve2(235741, 706948)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")

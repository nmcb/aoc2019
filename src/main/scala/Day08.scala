import scala.io.Source

object Day08 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val sizeY = 6
  val sizeX = 25

  val input: String =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim

  type Dig     = Int
  type Line[A] = List[A]

  case class Layer[A](digs: List[Line[A]]):

    def count(d: Dig): Int =
      digs.foldLeft(0)((a, l) => a + l.count(_ == d))

    def count0: Int =
      count(0)

    def count1: Int =
      count(1)

    def count2: Int =
      count(2)

    def map[B](f: A => B): Layer[B] =
      Layer(digs.map(_.map(f)))

    def zip[B](that: Layer[B]): Layer[(A,B)] =
      Layer(digs.zip(that.digs).map((da, db) => da.zip(db)))

  object Layer:

    def fill[A](a: A): Layer[A] =
      Layer((0 to sizeY).toList.map(_ => (0 to sizeX).toList.map(_ => a)))

  type Image[A] = List[Layer[A]]

  val image: Image[Dig] =
    input
      .toList
      .map(_.toString.toInt)
      .grouped(sizeX).toList
      .grouped(sizeY).toList
      .map(Layer[Dig].apply)

  val start1  = System.currentTimeMillis
  val answer1 = image.sortWith(_.count0 < _.count0).headOption.map(l => l.count1 * l.count2).get
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  // Part 2

  enum Pix:
    case Black
    case Trans
    case White

    def render: Char =
      this match
        case Black => '░'
        case White => '█'
        case Trans => ' '

    def stack(that: Pix): Pix =
      (this,that) match
        case (_, Black) => Black
        case (_, White) => White
        case (_, Trans) => this

  import Pix.*

  extension (dig: Dig)

    def digitToPix: Pix =
      dig match
        case 0 => Black
        case 1 => White
        case 2 => Trans
        case _ => sys.error("boom")

  def stack(top: Layer[Pix], bot: Layer[Pix]): Layer[Pix] =
    top.zip(bot).map((t,b) => b.stack(t))

  def render(layer: Layer[Pix]): String =
    layer.digs.map(line => line.map(_.render).mkString + "\n").mkString

  val start2  = System.currentTimeMillis
  val answer2 = render(image.map(l => l.map(_.digitToPix)).foldRight(Layer.fill(Trans))(stack))
  println(s"Day $day answer part 2:\n$answer2 [${System.currentTimeMillis - start1}ms]")

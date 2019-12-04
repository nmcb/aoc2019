package aoc

object Day04 {

  // Part 1

  def toDigits(i: Int): List[Int] = {
    def loop(i: Int, a: List[Int] = Nil): List[Int] =
      if (i < 0)
        a
      else if (i < 10)
        i :: a
      else
        loop(i / 10, (i % 10) :: a)

    loop(i)
  }

  assert(toDigits(123456) == List(1,2,3,4,5,6))

  def hasAdjecentDigits(as: List[Int]): Boolean = as match {
    case Nil                          => false
    case a1 :: a2 :: rest if a1 == a2 => true
    case a1 :: a2 :: Nil  if a1 == a2 => true
    case a1 :: rs                     => hasAdjecentDigits(rs)
  }

  assert(hasAdjecentDigits(List(1,2,3,4,5,6)) == false)
  assert(hasAdjecentDigits(List(1,1,3,4,5,6)) == true)
  assert(hasAdjecentDigits(List(1,2,3,4,5,5)) == true)
  assert(hasAdjecentDigits(List(1,1,1,1,1,1)) == true)
  
  def hasNonDecreasingDigits(as: List[Int]): Boolean = as match {
    case Nil                         => true
    case a1 :: a2 :: rest if a1 > a2 => false
    case a1 :: a2 :: Nil  if a1 > a2 => false
    case a1 :: rs                    => hasNonDecreasingDigits(rs)
  }

  assert(hasNonDecreasingDigits(List(1,2,3,4,5,6)) == true)
  assert(hasNonDecreasingDigits(List(1,2,3,4,6,5)) == false)
  assert(hasNonDecreasingDigits(List(2,1,3,4,6,5)) == false)
  assert(hasNonDecreasingDigits(List(2,2,3,4,5,0)) == false)

  val result1: Int =
    (235741 to 706948)
      .map(toDigits)
      .filter(hasAdjecentDigits)
      .filter(hasNonDecreasingDigits)
      .length
      
  // Part 2

  def hasTwoAdjecentDigitsGroup(as: List[Int]): Boolean = {
    val c = as.foldLeft(List.empty[List[Int]]) { (a, i) =>
      a match {
        case Nil                   => List(List(i))
        case h :: t if h.head == i => (i :: h) :: t
        case h :: t                => List(i) :: h :: t
      }
    }
    c.filter(g => g.length == 2).length >= 1
  }

  assert(hasTwoAdjecentDigitsGroup(List(1,2,3,4,5,6)) == false)
  assert(hasTwoAdjecentDigitsGroup(List(1,1,3,4,5,6)) == true)
  assert(hasTwoAdjecentDigitsGroup(List(1,1,1,2,2,2)) == false)
  assert(hasTwoAdjecentDigitsGroup(List(1,2,3,4,5,5)) == true)

  val result2: Int =
    (235741 to 706948)
      .map(toDigits)
      .filter(hasTwoAdjecentDigitsGroup)
      .filter(hasNonDecreasingDigits)
      .length
}
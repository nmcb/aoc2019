import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(3331849)(Day01.answer1)
    assertResult(4994530)(Day01.answer2)

  test("Day02"):
    assertResult(4576384)(Day02.answer1)
    assertResult(5398)(Day02.answer2)

  test("Day03"):
    assertResult(308)(Day03.answer1)
    assertResult(12934)(Day03.answer2)

  test("Day04"):
    assertResult(1178)(Day04.answer1)
    assertResult(763)(Day04.answer2)

  test("Day05"):
    assertResult(15386262)(Day05.answer1)
    assertResult(10376124)(Day05.answer2)

  test("Day06"):
    assertResult(162439)(Day06.answer1)
    assertResult(368)(Day06.answer2)

  test("Day07"):
    assertResult(21760)(Day07.answer1)
    assertResult(69816958)(Day07.answer2)

  test("Day08"):
    assertResult(1920)(Day08.answer1)
    assertResult(
      """███░░░██░░█░░█░█░░░░░██░░
        |█░░█░█░░█░█░░█░█░░░░█░░█░
        |█░░█░█░░░░█░░█░█░░░░█░░█░
        |███░░█░░░░█░░█░█░░░░████░
        |█░░░░█░░█░█░░█░█░░░░█░░█░
        |█░░░░░██░░░██░░████░█░░█░
        |""".stripMargin)(Day08.answer2)

  test("Day09"):
    assertResult(2399197539L)(Day09.answer1)
    assertResult(35106)(Day09.answer2)

  test("Day10"):
    assertResult(214)(Day10.answer1)
    assertResult(502)(Day10.answer2)

  test("Day11"):
    assertResult(1686)(Day11.answer1)
    assertResult(
      """░░██░░░██░░███░░███░░█░░█░████░█░░█░█░░░░░░
        |░█░░█░█░░█░█░░█░█░░█░█░█░░░░░█░█░░█░█░░░░░░
        |░█░░░░█░░█░█░░█░█░░█░██░░░░░█░░█░░█░█░░░░░░
        |░█░██░████░███░░███░░█░█░░░█░░░█░░█░█░░░░░░
        |░█░░█░█░░█░█░█░░█░░░░█░█░░█░░░░█░░█░█░░░░░░
        |░░███░█░░█░█░░█░█░░░░█░░█░████░░██░░████░░░
        |""".stripMargin)(Day11.answer2)

  test("Day12"):
    assertResult(13045)(Day12.answer1)
    assertResult(344724687853944L)(Day12.answer2)

  test("Day13"):
    assertResult(361)(Day13.answer1)
    assertResult(17590)(Day13.answer2)

  test("Day14"):
    assertResult(143173)(Day14.answer1)
    assertResult(8845261)(Day14.answer2)

  test("Day15"):
    assertResult(270)(Day15.answer1)
    assertResult(364)(Day15.answer2)

  test("Day16"):
    assertResult("27229269")(Day16.answer1)
    assertResult("26857164")(Day16.answer2)

  test("Day17"):
    assertResult(6448)(Day17.answer1)
    assertResult(914900)(Day17.answer2)

  test("Day18"):
    assertResult(3270)(Day18.answer1)
    assertResult(1628)(Day18.answer2)

  test("Day19"):
    assertResult(147)(Day19.answer1)
    assertResult(13280865)(Day19.answer2)

  test("Day20"):
    assertResult(644)(Day20.answer1)
    assertResult(7798)(Day20.answer2)

  test("Day21"):
    assertResult(19354392)(Day21.answer1)
    assertResult(1139528802)(Day21.answer2)

  test("Day22"):
    assertResult(7171)(Day21.answer1)
    assertResult(73394009116480L)(Day21.answer2)

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

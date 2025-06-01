import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(3331849)(Day01.answer1)
    assertResult(4994530)(Day01.answer2)

  test("Day02"):
    assertResult(4576384)(Day02.answer1)
    assertResult(5398)(Day02.answer2)

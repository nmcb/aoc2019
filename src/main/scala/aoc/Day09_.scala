package aoc

object Day09_ {

  type Program = List[Long]

  val program: Program = 
    List(1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,3,1,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,0,1020,1101,0,23,1010,1102,1,31,1009,1101,34,0,1019,1102,38,1,1004,1101,29,0,1017,1102,1,25,1018,1102,20,1,1005,1102,1,24,1008,1101,897,0,1024,1101,0,28,1016,1101,1,0,1021,1101,0,879,1028,1102,1,35,1012,1101,0,36,1015,1101,311,0,1026,1102,1,37,1011,1101,26,0,1014,1101,21,0,1006,1102,1,32,1002,1102,1,33,1003,1102,27,1,1001,1102,1,667,1022,1101,0,892,1025,1101,664,0,1023,1101,30,0,1000,1101,304,0,1027,1101,22,0,1013,1102,1,874,1029,1102,1,39,1007,109,12,21108,40,41,1,1005,1013,201,1001,64,1,64,1106,0,203,4,187,1002,64,2,64,109,5,1205,4,221,4,209,1001,64,1,64,1106,0,221,1002,64,2,64,109,5,21108,41,41,-5,1005,1017,243,4,227,1001,64,1,64,1106,0,243,1002,64,2,64,109,-30,2101,0,8,63,1008,63,30,63,1005,63,269,4,249,1001,64,1,64,1105,1,269,1002,64,2,64,109,15,2101,0,-5,63,1008,63,35,63,1005,63,293,1001,64,1,64,1106,0,295,4,275,1002,64,2,64,109,28,2106,0,-8,1001,64,1,64,1105,1,313,4,301,1002,64,2,64,109,-22,1205,7,329,1001,64,1,64,1106,0,331,4,319,1002,64,2,64,109,-12,1208,6,37,63,1005,63,351,1001,64,1,64,1106,0,353,4,337,1002,64,2,64,109,-3,2108,21,8,63,1005,63,375,4,359,1001,64,1,64,1106,0,375,1002,64,2,64,109,14,1201,-5,0,63,1008,63,39,63,1005,63,401,4,381,1001,64,1,64,1105,1,401,1002,64,2,64,109,17,1206,-9,419,4,407,1001,64,1,64,1105,1,419,1002,64,2,64,109,-10,21101,42,0,-4,1008,1015,42,63,1005,63,445,4,425,1001,64,1,64,1105,1,445,1002,64,2,64,109,-5,1206,7,457,1105,1,463,4,451,1001,64,1,64,1002,64,2,64,109,-6,2107,34,-5,63,1005,63,479,1105,1,485,4,469,1001,64,1,64,1002,64,2,64,109,-8,2102,1,5,63,1008,63,23,63,1005,63,505,1106,0,511,4,491,1001,64,1,64,1002,64,2,64,109,5,2102,1,1,63,1008,63,21,63,1005,63,537,4,517,1001,64,1,64,1105,1,537,1002,64,2,64,109,15,21107,43,44,-6,1005,1014,555,4,543,1106,0,559,1001,64,1,64,1002,64,2,64,109,-6,1207,-7,38,63,1005,63,579,1001,64,1,64,1106,0,581,4,565,1002,64,2,64,109,-17,1201,4,0,63,1008,63,28,63,1005,63,601,1106,0,607,4,587,1001,64,1,64,1002,64,2,64,109,14,2107,31,-9,63,1005,63,625,4,613,1105,1,629,1001,64,1,64,1002,64,2,64,109,15,21102,44,1,-7,1008,1019,44,63,1005,63,651,4,635,1106,0,655,1001,64,1,64,1002,64,2,64,109,3,2105,1,-6,1106,0,673,4,661,1001,64,1,64,1002,64,2,64,109,-14,21101,45,0,2,1008,1017,42,63,1005,63,693,1105,1,699,4,679,1001,64,1,64,1002,64,2,64,109,5,21107,46,45,-8,1005,1012,719,1001,64,1,64,1105,1,721,4,705,1002,64,2,64,109,-19,2108,21,7,63,1005,63,737,1106,0,743,4,727,1001,64,1,64,1002,64,2,64,109,9,1207,-2,25,63,1005,63,761,4,749,1106,0,765,1001,64,1,64,1002,64,2,64,109,-10,1208,1,27,63,1005,63,783,4,771,1106,0,787,1001,64,1,64,1002,64,2,64,109,5,1202,4,1,63,1008,63,29,63,1005,63,807,1106,0,813,4,793,1001,64,1,64,1002,64,2,64,109,8,21102,47,1,0,1008,1013,50,63,1005,63,833,1106,0,839,4,819,1001,64,1,64,1002,64,2,64,109,-12,1202,8,1,63,1008,63,31,63,1005,63,865,4,845,1001,64,1,64,1105,1,865,1002,64,2,64,109,34,2106,0,-7,4,871,1105,1,883,1001,64,1,64,1002,64,2,64,109,-18,2105,1,7,4,889,1105,1,901,1001,64,1,64,4,64,99,21101,0,27,1,21101,915,0,0,1106,0,922,21201,1,13801,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,942,1,0,1106,0,922,21201,1,0,-1,21201,-2,-3,1,21102,957,1,0,1105,1,922,22201,1,-1,-2,1106,0,968,21202,-2,1,-2,109,-3,2106,0,0)

  // Part 1
  sealed trait Mode
  case object IM extends Mode
  case object PM extends Mode
  case object RM extends Mode
  type Modes = (Mode, Mode, Mode)
  case class Op(code: Long, modes: Modes)
  object Op {
    def mode(i: Long): Mode = i match {
      case 0 => PM
      case 1 => IM
      case 2 => RM
      case _ => sys.error("boom")
    }

    def fromLong(i: Long): Op = {
      val code = i % 100
      val p1   = (i / 100) % 10
      val p2   = (i / 1000) % 10
      val p3   = (i / 10000) % 10
      Op(code, (mode(p1), mode(p2), mode(p3)))
    }
  }

  assert(Op.fromLong(1)     == Op(1,  (PM, PM, PM)))
  assert(Op.fromLong(2)     == Op(2,  (PM, PM, PM)))
  assert(Op.fromLong(3)     == Op(3,  (PM, PM, PM)))
  assert(Op.fromLong(4)     == Op(4,  (PM, PM, PM)))
  assert(Op.fromLong(99)    == Op(99, (PM, PM, PM)))
  assert(Op.fromLong(101)   == Op(1,  (IM, PM, PM)))
  assert(Op.fromLong(1002)  == Op(2,  (PM, IM, PM)))
  assert(Op.fromLong(11002) == Op(2,  (PM, IM, IM)))
  assert(Op.fromLong(201)   == Op(1,  (RM, PM, PM)))
  assert(Op.fromLong(21002) == Op(2,  (PM, IM, RM)))

  import java.util.concurrent._
  import scala.concurrent._

  class Comp(
    val name: String,
    val program: Program,
    var input: BlockingQueue[Long],
    val output: BlockingQueue[Long] = LinkedBlockingQueue[Long](),
    var halted: Boolean = false

  ) extends Thread {

    def interpret(prog: Program, i: Int = 0, b: Int = 0): Unit = {

      def op: Op  = Op.fromLong(prog(i))
      def v1: Long = if (op.modes._1 == PM) prog(prog(i+1).toInt) else if (op.modes._1 == IM) prog(i+1) else prog(b + prog(i+1).toInt)
      def v2: Long = if (op.modes._2 == PM) prog(prog(i+2).toInt) else if (op.modes._2 == IM) prog(i+2) else prog(b + prog(i+2).toInt)
      def v3: Long = if (op.modes._3 == PM) prog(prog(i+3).toInt) else if (op.modes._3 == IM) prog(i+3) else prog(b + prog(i+3).toInt)
  
      def calc(f: (Long, Long) => Long): Program =
        prog.updated(prog(i+3).toInt, f(v1, v2))
  
      def read(): Program = {
        val in = input.take() // blocking
        println(s"$name?> ${in}")
        prog.updated(prog(i+1).toInt, in)
      }
  
      def write(): Program = {
        println(s"$name!> ${v1}")
        output.put(v1)
        prog
      }
  
      def lt(): Program =
        if (v1 < v2) prog.updated(prog(i+3).toInt, 1) else prog.updated(prog(i+3).toInt, 0)
  
      def eq(): Program =
        if (v1 == v2) prog.updated(prog(i+3).toInt, 1) else prog.updated(prog(i+3).toInt, 0)

      op.code match {
        case 1  => interpret( calc(_+_)  , i+4, b )
        case 2  => interpret( calc(_*_)  , i+4, b )
        case 3  => interpret( read()     , i+2, b )
        case 4  => interpret( write()    , i+2, b )
        case 5  => if (v1 != 0) interpret( prog, v2.toInt, b ) else interpret( prog, i+3, b )
        case 6  => if (v1 == 0) interpret( prog, v2.toInt, b ) else interpret( prog, i+3, b )
        case 7  => interpret( lt(), i+4, b ) 
        case 8  => interpret( eq(), i+4, b )
        case 9  => interpret( prog, i+2, b + v1.toInt )
        case 99 => { halted = true ; () }
        case op: Long => throw new RuntimeException("Unknown opcode: " + op)
      }
    }

    def setIn(in: BlockingQueue[Long]): Unit = {
      while (Option(input.peek).nonEmpty) in.add(input.poll)
      input = in
    }

    def awaitHalt: Unit =
      while (!this.halted) Thread.sleep(10)

    override def run(): Unit =
      interpret(program)      

  }
  object Comp {
    def apply(name: String, program: Program, initInput: List[Long] = List.empty): Comp = {
      val input: BlockingQueue[Long] = LinkedBlockingQueue[Long]()
      Comp(name, program, initInput, input)
    }
    def apply(name: String, program: Program, initInput: List[Long], input: BlockingQueue[Long]): Comp = {
      initInput.forall(input.add(_) == true)
      val memory: List[Long] = program ++ List.fill(1024 * 1024)(0)
      new Comp(name, memory, input)
    }

  }

  def compareWith8(i: Long): Long = {
    val test: Program = List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    val comp = Comp("compareWith8", test, List(i))
    comp.start
    comp.output.take
  }

  assert( compareWith8(6) ==  999 )
  assert( compareWith8(7) ==  999 )
  assert( compareWith8(8) == 1000 )
  assert( compareWith8(9) == 1001 )

  def drain[A](queue: BlockingQueue[A], acc: List[A] = Nil): List[A] = Option(queue.poll()) match {
    case Some(a) => drain(queue, a :: acc)
    case None    => acc.reverse
  }
    
  val testQuinn: Unit = {
    val quinn = List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(_.toLong)
    val comp = Comp("testQuinn", quinn)
    comp.start
    comp.awaitHalt
    assert(drain(comp.output) == quinn)
  }

  val testLong: Unit = {
    val llong = List(104,1125899906842624l,99)
    val comp = Comp("testLong", llong)
    comp.start
    comp.awaitHalt
    assert(drain(comp.output) == List(1125899906842624l))
  }

  val test16Digit: Unit = {
    val prog = List(1102,34915192,34915192,7,4,7,99,0).map(_.toLong)
    val comp = Comp("test16Digit", prog)
    comp.start
    comp.awaitHalt
    assert(drain(comp.output).head.toString.length == 16)
  }
  
  val result1 = {
    val comp = Comp("result1", program, List(1L))
    comp.start
    comp.awaitHalt
    drain(comp.output)
  }
  
}
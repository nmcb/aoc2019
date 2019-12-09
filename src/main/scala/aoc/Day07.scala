package aoc

object Day07 {

  val program: List[Int] = 
    List(3,8,1001,8,10,8,105,1,0,0,21,42,55,64,77,94,175,256,337,418,99999,3,9,102,4,9,9,1001,9,5,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,101,5,9,9,4,9,99,3,9,1002,9,4,9,4,9,99,3,9,102,4,9,9,101,5,9,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99)

  // Part 1
  sealed trait Mode
  case object IM extends Mode
  case object PM extends Mode
  type Modes = (Mode, Mode, Mode)
  case class Op(code: Int, modes: Modes)
  object Op {
    def mode(i: Int): Mode =
      if (i == 0) PM else if (i == 1) IM else sys.error("boom")

    def fromInt(i: Int): Op = {
      val code = i % 100
      val p1   = (i / 100) % 10
      val p2   = (i / 1000) % 10
      val p3   = (i / 10000) % 10
      Op(code, (mode(p1), mode(p2), mode(p3)))
    }
  }

  assert(Op.fromInt(1)     == Op(1,  (PM, PM, PM)))
  assert(Op.fromInt(2)     == Op(2,  (PM, PM, PM)))
  assert(Op.fromInt(3)     == Op(3,  (PM, PM, PM)))
  assert(Op.fromInt(4)     == Op(4,  (PM, PM, PM)))
  assert(Op.fromInt(99)    == Op(99, (PM, PM, PM)))
  assert(Op.fromInt(101)   == Op(1,  (IM, PM, PM)))
  assert(Op.fromInt(1002)  == Op(2,  (PM, IM, PM)))
  assert(Op.fromInt(11002) == Op(2,  (PM, IM, IM)))

  import java.util.concurrent._
  import scala.concurrent._

  class Amp(
    val name: String,
    val program: List[Int],
    var input: BlockingQueue[Int],
    val output: BlockingQueue[Int] = LinkedBlockingQueue[Int](),
    var halted: Boolean = false

  ) extends Thread {

    def interpret(prog: List[Int], i: Int = 0): Unit = {

      def op: Op  = Op.fromInt(prog(i))
      def v1: Int = if (op.modes._1 == PM) prog(prog(i+1)) else prog(i+1)
      def v2: Int = if (op.modes._2 == PM) prog(prog(i+2)) else prog(i+2)
      def v3: Int = if (op.modes._3 == PM) prog(prog(i+3)) else prog(i+3)
  
      def calc(f: (Int, Int) => Int): List[Int] =
        prog.updated(prog(i+3), f(v1, v2))
  
      def read(): List[Int] = {
        val in = input.take() // blocking
        println(s"$name?> ${in}")
        prog.updated(prog(i+1), in)
      }
  
      def write(): List[Int] = {
        println(s"$name!> ${v1}")
        output.put(v1)
        prog
      }
  
      def lt(): List[Int] =
        if (v1 < v2) prog.updated(prog(i+3), 1) else prog.updated(prog(i+3), 0)
  
      def eq(): List[Int] =
        if (v1 == v2) prog.updated(prog(i+3), 1) else prog.updated(prog(i+3), 0)

      op.code match {
        case 1  => interpret( calc(_+_)   , i+4 )
        case 2  => interpret( calc(_*_)   , i+4 )
        case 3  => interpret( read()      , i+2 )
        case 4  => interpret( write()     , i+2 )
        case 5  => if (v1 != 0) interpret( prog, v2 ) else interpret( prog, i+3 )
        case 6  => if (v1 == 0) interpret( prog, v2 ) else interpret( prog, i+3 )
        case 7  => interpret( lt(), i+4 ) 
        case 8  => interpret( eq(), i+4 )
        case 99 => synchronized { halted = true ; () }
        case op: Int => throw new RuntimeException("Unknown opcode: " + op)
      }
    }

    def setIn(in: BlockingQueue[Int]): Unit = {
      while (Option(input.peek).nonEmpty) in.add(input.poll)
      input = in
    }

    override def run(): Unit =
      interpret(program)      

  }
  object Amp {
    def apply(name: String, program: List[Int], initInput: List[Int] = List.empty): Amp = synchronized {
      val input: BlockingQueue[Int] = LinkedBlockingQueue[Int]()
      Amp(name, program, initInput, input)
    }
    def apply(name: String, program: List[Int], initInput: List[Int], input: BlockingQueue[Int]): Amp = synchronized {
      initInput.forall(input.add(_) == true)
      new Amp(name, program, input)
    }

  }

  def compareWith8(i: Int): Int = {
    val test: List[Int] = List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    val a1 = Amp("test", test, List(i))
    a1.start
    a1.output.take
  }

  assert( compareWith8(6) ==  999 )
  assert( compareWith8(7) ==  999 )
  assert( compareWith8(8) == 1000 )
  assert( compareWith8(9) == 1001 )

  def configuration1(phases: List[Int], prog: List[Int], init: Int): Int = {
    assert(phases.length == 5)
    assert((0 to 4).forall(phases.contains(_)))

    val a = Amp("a", program, List(phases(0), init))
    val b = Amp("b", program, List(phases(1)), a.output)
    val c = Amp("c", program, List(phases(2)), b.output)
    val d = Amp("d", program, List(phases(3)), c.output)
    val e = Amp("e", program, List(phases(4)), d.output)

    a.start
    b.start
    c.start
    d.start
    e.start

    while (!e.halted) Thread.sleep(10)

    e.output.take
  }

  val result1: Int = // 21760
    List(0,1,2,3,4)
      .permutations.toList
      .map(ps => configuration1(ps, program, 0))
      .max

  assert( result1 == 21760 )

  // Part 2

  def configuration2(phases: List[Int], prog: List[Int], init: Int): Int = {
    assert(phases.length == 5)
    assert((5 to 9).forall(phases.contains(_)))

    lazy val a: Amp = Amp("a", program, List(phases(0), init))
    lazy val b: Amp = Amp("b", program, List(phases(1)), a.output)
    lazy val c: Amp = Amp("c", program, List(phases(2)), b.output)
    lazy val d: Amp = Amp("d", program, List(phases(3)), c.output)
    lazy val e: Amp = Amp("e", program, List(phases(4)), d.output)

    a.setIn(e.output)

    a.start
    b.start
    c.start
    d.start
    e.start

    while (!e.halted) Thread.sleep(10)
    
    println(s"a halted: ${a.halted}")
    println(s"b halted: ${b.halted}")
    println(s"c halted: ${c.halted}")
    println(s"d halted: ${d.halted}")
    println(s"e halted: ${e.halted}")

    e.output.take
  }


  val result2: Int =
    List(5,6,7,8,9)
      .permutations.toList
      .map(ps => configuration2(ps, program, 0))
      .max

  assert( result2 == 69816958 )
        
  }
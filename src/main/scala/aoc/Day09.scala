package aoc

object Day09 {

  sealed trait Mode
  case object IM extends Mode
  case object PM extends Mode
  case object RM extends Mode
  type Modes = (Mode, Mode, Mode)
  case class Op(code: Int, modes: Modes)
  object Op {
    def mode(i: Int): Mode = i match {
      case 0 => PM
      case 1 => IM
      case 2 => RM
      case _ => sys.error("boom")
    }

    def fromLong(l: Long): Op = {
      val i = l.toInt
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

  import java.util.concurrent._
  import scala.concurrent._

  type Mem = Array[Int]
  type Sys = BlockingQueue[Long]

  class Prg(
    val name: String,
    val memory: Mem,
    var input: Sys,
    val output: Sys = LinkedBlockingQueue[Long](),
    @volatile var halted: Boolean = false

  ) extends Thread {

    def interpret(mem: Mem, i: Int = 0, b: Int = 0): Unit = {

      def op: Op  = Op.fromLong(mem(i))
      def value(m: Mode, address: Int) = m match {
        case PM => mem(mem(address))
        case IM => mem(address)
        case RM => mem(mem(address) + b)
      }
      def value1: Int = value(op.modes._1, i+1)
      def value2: Int = value(op.modes._2, i+2)
      def value3: Int = value(op.modes._3, i+3)

      def setMem(addr: Int /* truncated to a 32 Bit address space */, value: Int): Mem =
        mem.updated(addr.toInt, value)

        
  
      def calc(f: (Int, Int) => Int): Mem =
        setMem(mem(i+3), f(value1, value2))
  
      def read(): Mem = {
        val in = input.take() // blocking
        println(s"$name?> ${in}")
        setMem(mem(i+1), in.toInt)
      }
  
      def write(): Mem = {
        println(s"$name!> ${value1}")
        output.put(value1)
        mem
      }
  
      def lt(): Mem =
        if (value1 < value2) setMem(mem(i+3), 1) else setMem(mem(i+3), 0)
  
      def eq(): Mem =
        if (value1 == value2) setMem(mem(i+3), 1) else setMem(mem(i+3), 0)

      op.code match {
        case 1  => interpret( calc(_+_)   , i+4 , b)
        case 2  => interpret( calc(_*_)   , i+4 , b)
        case 3  => interpret( read()      , i+2 , b)
        case 4  => interpret( write()     , i+2 , b)
        case 5  => if (value1 != 0) interpret( mem, value2 , b) else interpret( mem, i+3 , b)
        case 6  => if (value1 == 0) interpret( mem, value2 , b) else interpret( mem, i+3 , b)
        case 7  => interpret( lt(), i+4 , b) 
        case 8  => interpret( eq(), i+4 , b)
        case 9  => interpret( mem, i+2 , b+value1 )
        case 99 => halted = true
        case op: Int => throw new RuntimeException("Unknown opcode: " + op)
      }
    }

    def setIn(in: Sys): Unit = {
      while (Option(input.peek).nonEmpty) in.add(input.poll)
      input = in
    }

    override def run(): Unit =
      interpret(memory)      

  }
  object Prg {
    def apply(name: String, program: Mem, initInput: Mem = Array.empty): Prg = {
      val input: Sys = LinkedBlockingQueue[Long]()
      Prg(name, program, initInput, input)
    }
    def apply(name: String, program: Mem, initInput: Mem, input: Sys): Prg = {
      initInput.forall(input.add(_) == true)
      new Prg(name, program ++ Array.fill(1024 + 1024)(0), input)
    }
    def drain(queue: Sys, acc: List[Long] = Nil): List[Long] = Option(queue.poll()) match {
      case Some(i) => drain(queue, i :: acc)
      case None    => acc.reverse
    }
    def await(prg: Prg): Unit =
      while (!prg.halted) Thread.sleep(10)
  }

  def compareWith8(i: Int): Long = {
    val test: Mem = Array(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    val a1 = Prg("test", test, Array(i))
    a1.start
    a1.output.take
  }

  assert( compareWith8(6) ==  999 )
  assert( compareWith8(7) ==  999 )
  assert( compareWith8(8) == 1000 )
  assert( compareWith8(9) == 1001 )

  val programDay7: Mem = 
    Array(3,8,1001,8,10,8,105,1,0,0,21,42,55,64,77,94,175,256,337,418,99999,3,9,102,4,9,9,1001,9,5,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,101,5,9,9,4,9,99,3,9,1002,9,4,9,4,9,99,3,9,102,4,9,9,101,5,9,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99)

  def configuration1(phases: Mem, init: Int): Long = {
    assert(phases.length == 5)
    assert((0 to 4).forall(phases.contains(_)))

    val a = Prg("a", programDay7, Array(phases(0), init))
    val b = Prg("b", programDay7, Array(phases(1)), a.output)
    val c = Prg("c", programDay7, Array(phases(2)), b.output)
    val d = Prg("d", programDay7, Array(phases(3)), c.output)
    val e = Prg("e", programDay7, Array(phases(4)), d.output)

    a.start
    b.start
    c.start
    d.start
    e.start

    while (!e.halted) Thread.sleep(10)

    e.output.take
  }

  val day7Result1: Long =
    Array(0,1,2,3,4)
      .permutations
      .map(ps => configuration1(ps, 0))
      .max

  assert( day7Result1 == 21760 )

  def configuration2(phases: Mem, init: Int): Long = {
    assert(phases.length == 5)
    assert((5 to 9).forall(phases.contains(_)))

    lazy val a: Prg = Prg("a", programDay7, Array(phases(0), init))
    lazy val b: Prg = Prg("b", programDay7, Array(phases(1)), a.output)
    lazy val c: Prg = Prg("c", programDay7, Array(phases(2)), b.output)
    lazy val d: Prg = Prg("d", programDay7, Array(phases(3)), c.output)
    lazy val e: Prg = Prg("e", programDay7, Array(phases(4)), d.output)

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
  val day7Result2 =
    Array(5,6,7,8,9)
      .permutations
      .map(ps => configuration2(ps, 0))
      .max

  assert( day7Result2 == 69816958 )

  // Day 9 - Part 1

  val testOp9AndRMOp4 = {
    // SRB:IM 2019  --b: 0      -> 2019     ; b+19
    // OUT:RM  -35  -->: [1984] -> 666      ; b-35
    // HLT
    val prg = Prg("testOp9AndRMOp4", 
      //        0      1      2      3     4      5        ...       1983         1984  
      Array(  109,  2019,   204,   -35,   99) ++ (0  to 1978).map(_ => 0) ++ List(666), Array.empty)
    val run = prg.start
    Prg.await(prg)
    assert( prg.output.take == 666 )
  }

  val testQuinn = {
    val quinn = Array(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    val prg = Prg("testOp9AndRMOp4", quinn, Array.empty)
    val run = prg.start
    Prg.await(prg)
    assert( Prg.drain(prg.output) == quinn.toList )
  }
        
  val test16DigitOutput = {
    val multiplyTwoEightDigitNumbers = Array(1102,34915192,34915192,7,4,7,99,0)
    val prg = Prg("testOp9AndRMOp4", multiplyTwoEightDigitNumbers, Array.empty)
    val run = prg.start
    Prg.await(prg)
    assert( prg.output.take.toString.length == 16 )
  }
}
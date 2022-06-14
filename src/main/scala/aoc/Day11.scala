 package aoc

 object Day11 {

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

   import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
   import scala.concurrent._
   import scala.concurrent.duration._

   implicit val ec: ExecutionContext = ExecutionContext.global

   case class Prog(name: String, mem: Prog.Mem, sys: Prog.Sys)

   object Prog {

     type Mem    = Array[Long]
     type Stream = LinkedBlockingQueue[Long]
     type Addr   = Int

     case class Sys(input: Stream = LinkedBlockingQueue[Long], output: Stream  = LinkedBlockingQueue[Long])

     def drain(stream: Stream, acc: List[Long] = Nil): List[Long] = Option(stream.poll()) match {
       case Some(i) => drain(stream, i :: acc)
       case None    => acc.reverse
     }


     def mk(name: String, program: Mem, initInput: Mem, sys: Sys
     ): Prog = {
       initInput.forall(sys.input.add(_) == true)
       new Prog(name, program ++ Array.fill(1024 + 1024)(0L), sys)
     }


     @scala.annotation.tailrec
     final def interpret(mem: Mem, sys: Sys, pc: Addr = 0, base: Addr = 0): Unit = {

       val op: Op  = Op.fromLong(mem(pc))
       def value1: Long = get(op.modes._1, pc+1)
       def value2: Long = get(op.modes._2, pc+2)
       def value3: Long = get(op.modes._3, pc+3)

       def addr1: Addr = address(op.modes._1, pc+1)
       def addr2: Addr = address(op.modes._2, pc+2)
       def addr3: Addr = address(op.modes._3, pc+3)

       def address(mode: Mode, addr: Addr): Addr = mode match {
         case PM => mem(addr).toInt
         case IM => addr
         case RM => mem(addr).toInt + base
       }

       def get(mode: Mode, addr: Addr): Long =
         mem(address(mode, addr))

       def set(addr: Addr, value: Long): Mem =
         mem.updated(addr.toInt, value)
  
       def calc(op: (Long, Long) => Long): Mem =
         set(addr3, op(value1, value2))
  
       def read(): Mem = {
         val in = sys.input.take() // blocking
         set(addr1, in)
       }
  
       def write(): Mem = {
         sys.output.put(value1)
         mem
       }
  
       def lt(): Mem =
         if (value1 < value2) set(addr3, 1) else set(addr3, 0)
  
       def eq(): Mem =
         if (value1 == value2) set(addr3, 1) else set(addr3, 0)

       def debug: Unit =
         println(s"pc=$pc, base=$base, op=$op, addr=[$addr1, $addr2, $addr3]")

       op.code match {
         case 1  => interpret(calc(_+_) , sys, pc+4, base)
         case 2  => interpret(calc(_*_) , sys, pc+4, base)
         case 3  => interpret(read()    , sys, pc+2, base)
         case 4  => interpret(write()   , sys, pc+2, base)
         case 5  => if (value1 != 0) interpret(mem, sys, value2.toInt , base) else interpret( mem, sys, pc+3 , base)
         case 6  => if (value1 == 0) interpret(mem, sys, value2.toInt , base) else interpret( mem, sys, pc+3 , base)
         case 7  => interpret(lt(), sys, pc+4, base)
         case 8  => interpret(eq(), sys, pc+4, base)
         case 9  => interpret(mem , sys, pc+2, (base+value1).toInt)
         case 99 => ()
         case op: Int => throw new RuntimeException("Unknown opcode: " + op)
       }
     }
   }

   val mem: Prog.Mem =
     Array(3L,8,1005,8,306,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,28,2,107,3,10,1,101,19,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,59,2,5,13,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,85,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,107,1006,0,43,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,132,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,154,2,4,1,10,2,4,9,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,183,1,1102,5,10,1,1102,1,10,1006,0,90,2,9,12,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,221,1006,0,76,1006,0,27,1,102,9,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,252,2,4,9,10,1006,0,66,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,282,1,102,19,10,101,1,9,9,1007,9,952,10,1005,10,15,99,109,628,104,0,104,1,21102,1,387240010644L,1,21101,0,323,0,1105,1,427,21102,846541370112L,1,1,21101,334,0,0,1106,0,427,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,3425718295L,1,1,21102,381,1,0,1105,1,427,21102,179410541715L,1,1,21101,0,392,0,1106,0,427,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,718078255872L,1,21101,0,415,0,1105,1,427,21102,1,868494234468L,1,21102,1,426,0,1105,1,427,99,109,2,21202,-1,1,1,21101,0,40,2,21101,458,0,3,21101,0,448,0,1106,0,491,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,453,454,469,4,0,1001,453,1,453,108,4,453,10,1006,10,485,1102,0,1,453,109,-2,2105,1,0,0,109,4,2102,1,-1,490,1207,-3,0,10,1006,10,508,21102,1,0,-3,22102,1,-3,1,22101,0,-2,2,21102,1,1,3,21102,1,527,0,1106,0,532,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,555,2207,-4,-2,10,1006,10,555,22101,0,-4,-4,1105,1,623,22101,0,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,574,0,0,1105,1,532,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,593,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,615,21201,-1,0,1,21101,615,0,0,106,0,490,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0)

   case class Color(id: Long, char: Char) { override val toString: String = char.toString }
   val Black = Color(0, '·')
   val White = Color(1, '#')

   case class Turn(id: Long)
   val L = Turn(0)
   val R = Turn(1)

   case class Loc(x: Int = 6, y: Int = 6) {
     def advance(d: Dir): Loc = d match {
       case N => Loc(x, y - 1)
       case E => Loc(x + 1, y)
       case S => Loc(x, y + 1)
       case W => Loc(x - 1, y)
     }
   }

   sealed trait Dir {
     def turn(t: Turn): Dir = (this, t) match {
       case (N, L) => W
       case (N, R) => E
       case (E, L) => N
       case (E, R) => S
       case (S, L) => E
       case (S, R) => W
       case (W, L) => S
       case (W, R) => N
       case _ => sys.error("boom")
     }
   }
   object N extends Dir
   object E extends Dir
   object S extends Dir
   object W extends Dir

   case class Space(coords: List[List[(Color, Boolean)]] = List.fill(21)(List.fill(21)((Black, false)))) {

     def get(loc: Loc): Color =
       coords(loc.y)(loc.x)._1

     def set(loc: Loc)(color: Color): Space =
       Space(coords.zipWithIndex.map((l,y) => l.zipWithIndex.map((c,x) => if (loc.x == x && loc.y == y) (color, true) else c)))

     def nrSetAtLeastOnce: Int =
       coords.flatten.filter(_._2 == true).length
    
     override def toString: String =
       "\n" + coords.map(_.map(_._1).mkString).mkString("\n")

   }

   case class Robot(
     name: String,
     prog: Prog = Prog("prog", mem, Prog.Sys()),
     @volatile var halted: Boolean = false,
     @volatile var nrSetAtLeastOnce: Int = 0
  
   ) extends Thread { // with Await {

     println("constructing robot")

     def read(): Long = {
       print(s"$name?> ")
       val in = prog.sys.output.take() // blocking
       println(in)
       in
     }

     def write(l: Long): Unit = {
       print(s"$name!>")
       prog.sys.input.put(l)
       println(s"${l}")
     }

     @scala.annotation.tailrec
     final def paint(space: Space = Space(), loc: Loc = Loc(), dir: Dir = N): Space = {

       System.out.println(s"loc=$loc, dir=$dir:\n$space")

       write(space.get(loc).id)

       val s = space.set(loc)(read() match {
         case White.id => White
         case Black.id => Black
         case _ => sys.error("boom")
       })

       val d = read() match {
         case L.id => dir.turn(L)
         case R.id => dir.turn(R)
         case _ => sys.error("boom")
       }

       val l = loc.advance(d)

       if (halted) s else paint(s, l, d)
     }

     override def run(): Unit = {
       // prog.start
       paint()
       println(s"nr=${paint().nrSetAtLeastOnce}")
       // Await.await(prog)
     }
   }

   object Rob {

     @scala.annotation.tailrec
     def paint(name: String, i: Int = 0, space: Space = Space(), loc: Loc = Loc(), dir: Dir = N): Space = {
       println(s"name=$name, thread=${Thread.currentThread.getId}")
       println(s"loc=$loc, dir=$dir:\n$space")

       if (i < 5) paint(name, i+1, space) else space
     }

     def mk(name: String = "rob"): Future[Space] =
       Future { paint(name) }
   }
  
   val space: Future[Space] = Rob.mk()

   def result =
     Await.result(space, 1.seconds)

 }
package cpu

type Value        = Long
type Pointer      = Int
type PointerValue = (Pointer, Value)

extension (pv: PointerValue)
  def pointer: Pointer = pv._1
  def value: Value     = pv._2

case class Mem(underlying: Map[Pointer, Value]):
  def apply(p: Pointer): Value           = underlying(p)
  def updated(p: Pointer, v: Value): Mem = Mem(underlying.updated(p, v))
  def +(pv: PointerValue): Mem           = updated(pv.pointer, pv.value)

object Mem:

  def apply(values: Value*): Mem =
    Mem(values.view.zipWithIndex.map((v, i) => i -> v).toMap.withDefaultValue(0L))

  def parse(s: String): Mem =
    Mem(s.split(',').map(_.toLong).toSeq *)

type State = (CPU,Option[Value])

extension (state: State)
  def cpu: CPU                    = state._1
  def outputOption: Option[Value] = state._2

case class CPU(mem: Mem, stdin: LazyList[Value] = LazyList.empty, ip: Pointer = 0, base: Pointer = 0):

  def value(offset: Int): Value   = mem(ip + offset)
  def opcode: Int                 = (value(0) % 100).toInt
  def param(offset: Int): Value   = value(offset + 1)
  def paramMode(offset: Int): Int = ((value(0) / math.pow(10, 2 + offset).toInt) % 10).toInt

  def read(i: Int): Value =
    paramMode(i) match
      case 0 => mem(param(i).toInt)
      case 1 => param(i)
      case 2 => mem((base + param(i)).toInt)
      case _ => sys.error(s"illegal parameter read mode ${paramMode(i)}")

  def write(i: Int, value: Value): Mem =
    paramMode(i) match
      case 0 => mem.updated(param(i).toInt, value)
      case 2 => mem.updated((base + param(i)).toInt, value)
      case _ => sys.error(s"illegal parameter write mode ${paramMode(i)}")

  def executeOne: Option[State] =
    opcode match
      case  1 => Some((copy(mem = write(2, read(0) + read(1)), ip = ip + 4), None))
      case  2 => Some((copy(mem = write(2, read(0) * read(1)), ip = ip + 4), None))
      case  3 => Option.when(stdin.nonEmpty)(copy(mem = write(0, stdin.head), ip = ip + 2, stdin = stdin.tail), None)
      case  4 => Some((copy(ip = ip + 2), Some(read(0))))
      case  5 => if read(0) != 0 then Some((copy(ip = read(1).toInt), None)) else Some((copy(ip = ip + 3), None))
      case  6 => if read(0) == 0 then Some((copy(ip = read(1).toInt), None)) else Some((copy(ip = ip + 3), None))
      case  7 => Some((copy(mem = write(2, if (read(0)  < read(1)) 1 else 0), ip = ip + 4), None))
      case  8 => Some((copy(mem = write(2, if (read(0) == read(1)) 1 else 0), ip = ip + 4), None))
      case  9 => Some((copy(ip = ip + 2, base = base + read(0).toInt), None))
      case 99 => None
      case _  => sys.error(s"unknown opcode $opcode")

  def outputs: LazyList[Value] =
    LazyList.unfold(this)(_.executeOne.map(_.swap)).flatten

  def executeAll: LazyList[State] =
    LazyList.unfold(this)(state => state.executeOne.map(next => (next, next.cpu)))

  def outputStates: LazyList[(CPU,Value)] =
    executeAll.flatMap((state, output) => output.map((state, _)))

  def execFinal: CPU =
    executeAll.last.cpu
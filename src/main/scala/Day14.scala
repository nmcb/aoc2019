import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Molecule  = String
  type Amount    = Long
  type Molecules = Map[Molecule,Amount]
  type Dose      = (Molecule,Amount)

  extension (amount: Amount)
    infix def /^(that: Amount) =
      if amount % that == 0 then amount / that else amount / that + 1

  extension (dose: Dose)
    def molecule: Molecule = dose._1
    def amount: Amount     = dose._2

  object Dose:
    def fromString(s: String): Dose =
      val Array(amount, molecule) = s.split(" ")
      (molecule, amount.toLong)

  object Molecules:
    def fromString(s: String): Molecules =
      s.split(", ").map(Dose.fromString).toMap

  case class Reaction(from: Molecules, amount: Amount)

  type Reactions = Map[Molecule,Reaction]

  extension (molecules: Molecules)

    def modify(molecule: Molecule, amount: Amount): Molecules =
      molecules.updated(molecule, molecules(molecule) + amount)

  extension (reactions: Reactions)

    def makeFuel(amount: Amount): Amount =

      def make(molecule: Molecule, amount: Amount, required: Molecules): Molecules =
        if required(molecule) >= amount || molecule == "ORE" then
          required.modify(molecule, -amount)
        else
          val additional = amount - required(molecule)
          val multiplier = additional /^  reactions(molecule).amount
          reactions(molecule)
            .from
            .foldLeft(required):
              case (molecules,(molecule,amount)) => make(molecule, amount * multiplier, molecules)
            .modify(molecule, multiplier * reactions(molecule).amount - amount)

      val result = make("FUEL", amount, Map.empty.withDefaultValue(0L))
      -result("ORE")

    def makeMaxFuel(oreThreshhold: Amount): Amount =

      def binarySearch(start: Amount, end: Amount): Amount =
        if start >= end then
          start
        else
          val middle = (start + end) / 2
          val cost   = reactions.makeFuel(middle)
          if cost > oreThreshhold then
            binarySearch(start, middle - 1)
          else if cost < oreThreshhold then
            binarySearch(middle + 1, end)
          else
            middle

      binarySearch (1, oreThreshhold)


  val reactions: Reactions =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"$from => $to" =>
          val (molecule, amount) = Dose.fromString(to)
          molecule -> Reaction(from = Molecules.fromString(from), amount = amount)
      .toMap

  val start1  = System.currentTimeMillis
  val answer1 = reactions.makeFuel(1)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")



  val start2  = System.currentTimeMillis
  val answer2 = reactions.makeMaxFuel(1000000000000L)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

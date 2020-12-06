import scala.annotation.tailrec
import scala.io.Source

object Day06:

  object T:
    opaque type Declaration = Set[Char]

    object Declaration:
      def apply(s: String): Declaration = s.toSet

    opaque type Group = List[Declaration]

    object Group:
      def apply(decls: List[Declaration] = Nil): Group = decls
      def apply(decl: Declaration): Group = List(decl)

    extension (g: Group)
      def add(decl: Declaration): Group = decl :: g
      def anyYesCount: Int = g.foldLeft(Set.empty[Char])(_ union _).size
      def allYesCount: Int = g.reduce(_ intersect _).size

  import T._

  def parseInput(): List[Group] =
    val lines = Source.fromFile("./data/day_06.txt").getLines

    @tailrec def loop(acc: List[Group], lines: Iterator[String]): List[Group] =
      if lines.hasNext then
        lines.next match
          case "" =>
            loop(Group() :: acc, lines)
          case line =>
            val decl: Declaration = Declaration(line)
            acc match
              case group :: groups =>
                loop((group add decl) :: groups, lines)
              case Nil =>
                loop(List(Group(decl)), lines)
      else
        acc

    loop(Nil, lines)

  val input = parseInput()

  def part1: Int = input.map(_.anyYesCount).sum

  def part2: Int = input.map(_.allYesCount).sum

  def main(args: Array[String]) =
    println(part1)
    println(part2)

import scala.annotation.tailrec
import scala.io.Source

object Day06 {

  opaque type Declaration = Set[Char]

  opaque type Group = List[Declaration]

  extension (g: Group)
    def anyYesCount: Int =
      g.foldLeft(Set.empty[Char])(_ union _).size

    def allYesCount: Int =
      g.reduce(_ intersect _).size

  def parseInput(): List[Group] =
    val lines = Source.fromFile("./data/day_06.txt").getLines

    @tailrec def loop(acc: List[Group], lines: Iterator[String]): List[Group] =
      if lines.hasNext then
        lines.next match
          case "" =>
            loop(Nil :: acc, lines)
          case line =>
            val decl = line.toSet
            acc match
              case decls :: groups =>
                loop((decl :: decls) :: groups, lines)
              case Nil =>
                loop(List(List(decl)), lines)
      else
        acc

    loop(Nil, lines)

  val input = parseInput()

  def part1: Int =
    input.map(_.anyYesCount).sum

  def part2: Int =
    input.map(_.allYesCount).sum

  def main(args: Array[String]) =
    println(part1)
    println(part2)
}

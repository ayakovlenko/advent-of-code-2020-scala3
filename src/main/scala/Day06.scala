import scala.annotation.tailrec
import scala.io.Source

object Day06 {

  case class Group(decls: List[Declaration]) {

    def anyYesCount: Int =
      decls.foldLeft(Set.empty[Char]) { (acc, decl) =>
        acc ++ decl.yesAnswers
      }.size

    def allYesCount: Int =
      decls
        .map(_.yesAnswers)
        .reduce(_ intersect _)
        .size
  }

  case class Declaration(yesAnswers: Set[Char])

  def parseInput(): List[Group] =
    val lines = Source.fromFile("./data/day_06.txt").getLines

    @tailrec def loop(acc: List[Group], lines: Iterator[String]): List[Group] =
      if lines.hasNext then
        lines.next match
          case "" =>
            loop(Group(Nil) :: acc, lines)
          case line =>
            val decl = Declaration(line.toSet)
            acc match
              case Group(decls) :: groups =>
                loop(Group(decl :: decls) :: groups, lines)
              case Nil =>
                loop(List(Group(List(decl))), lines)
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

import scala.annotation.tailrec
import scala.io.Source

object Day01:

  val input = Source.fromFile("./data/day_01.txt").getLines.map(_.toInt).toList

  extension (xs: List[Int])
    def twoSum(target: Int): Option[(Int, Int)] =
      @tailrec def loop(acc: Map[Int, Int], xs: List[Int]): Option[(Int, Int)] =
        xs match
          case x :: _ if acc.contains(x) => Some(x -> acc(x))
          case x :: ys => loop(acc + (target - x -> x), ys)
          case Nil => None
      loop(Map.empty, xs)

    def threeSum(target: Int): Option[(Int, Int, Int)] =
      val ys = xs.toSet
      xs.combinations(xs.size - 1)
        .map { c =>
          val x = ys.diff(c.toSet).head
          c.twoSum(2020 - x).map { (y, z) => (x, y, z) }
        }
        .dropWhile(_.isEmpty)
        .next()

  def product(tupleN: Product): Int =
    tupleN.productIterator.map(_.asInstanceOf[Int]).product
  
  def part1: Option[Int] =
    input.twoSum(2020).map(product)

  def part2: Option[Int] =
    input.threeSum(2020).map(product)

  def main(args: Array[String]) =
    part1.foreach(println)
    part2.foreach(println)

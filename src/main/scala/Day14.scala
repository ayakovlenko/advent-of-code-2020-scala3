import scala.io.Source
import scala.annotation.tailrec

object Day14:

  object T:

    enum MaskVal:
      case _0, _1, _X

    opaque type Mask = String

    object Mask:
      def apply(s: String): Mask = s

    extension (m: Mask)
      def values: List[MaskVal] =
        m.map {
          case '0' => MaskVal._0
          case '1' => MaskVal._1
          case 'X' => MaskVal._X
          case _ => throw new Error
        }.toList

    case class Program(mask: Mask, instructions: List[Instruction])

    case class Instruction(memAddr: Long, value: Long)

    extension (x: Long)
      def bits: List[Int] =
        var result = List.empty[Int]
        var y = x
        var i = 0
        while i < 36 do
          result = (y & 1).toInt :: result
          i = i + 1
          y = y >> 1
        result

    extension (xs: List[Int])
      def toLong: Long = java.lang.Long.parseLong(xs.mkString, 2)

  import T._

  object InputParser:

    private val MaskPattern = raw"mask = (.+)".r

    private val MemPattern = raw"mem\[(\d+)\] = (\d+)".r

    def parse(filename: String): List[Program] =
      Source.fromFile(filename).getLines
        .foldLeft[List[Program]](Nil) { (acc, ln) =>
          ln match
            case MaskPattern(s) =>
              val newProgram = Program(Mask(s), Nil)
              acc match
                case p@Program(m, insts) :: ps =>
                  newProgram :: Program(m, insts.reverse) :: ps
                case _ =>
                  newProgram :: acc
            case MemPattern(memAddr, value) =>
              acc match
                case Nil => throw new Error
                case Program(mask, insts) :: ps =>
                  Program(
                    mask,
                    Instruction(memAddr.toInt, value.toLong) :: insts
                  ) :: ps
        }.reverse
  
  // ---

  object Emulator1:

    def run(programs: List[Program]): Long =
      val mem = new Array[Long](
        _length = Math.toIntExact(
          programs.flatMap(_.instructions).map(_.memAddr).max + 1
        )
      )
      programs.foreach { program =>
        program.instructions.foreach { case Instruction(memAddr, value) =>
          val newValue =
            (value.bits zip program.mask.values).map {
              case (_, MaskVal._0) => 0
              case (_, MaskVal._1) => 1
              case (x, MaskVal._X) => x
            }.toLong
          mem(Math.toIntExact(memAddr)) = newValue
        }
      }
      mem.sum
    
  object Emulator2:

    def run(programs: List[Program]): Long =
      val mem = scala.collection.mutable.Map.empty[Long, Long]
      programs.foreach { program =>
        program.instructions.foreach { case Instruction(memAddr, value) =>
          floatingAddrs(memAddr, program.mask).foreach { memAddr =>
            mem(memAddr) = value
          }
        }
      }
      mem.values.sum

    def floatingAddrs(memAddr: Long, mask: Mask): List[Long] =
      def loop(xs: List[(Int, MaskVal)], bits: List[Int]): List[Long] =
        xs match
          case Nil =>
            bits.reverse.toLong :: Nil
          case (x, MaskVal._0) :: ys =>
            loop(ys, x :: bits)
          case (_, MaskVal._1) :: ys =>
            loop(ys, 1 :: bits)
          case (x, MaskVal._X) :: ys =>
            loop(ys, 0 :: bits) ++ loop(ys, 1 :: bits)

      loop(memAddr.bits zip mask.values, Nil)

    // ---

  def main(args: Array[String]): Unit =
    val input = InputParser.parse("./data/day_14.txt")

    println(Emulator1.run(input)) // == 9296748256641
    
    println(Emulator2.run(input)) // == 4877695371685

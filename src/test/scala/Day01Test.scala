import org.junit.Assert.assertEquals
import org.junit.Test

class Day01Test {

  @Test def part1(): Unit =
    assertEquals(Some(805731), Day01.part1)

  @Test def part2(): Unit =
    assertEquals(Some(192684960), Day01.part2)
}

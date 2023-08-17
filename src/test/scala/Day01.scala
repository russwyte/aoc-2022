import scala.io.Source

trait AdventOfCodeSuite extends munit.FunSuite { self =>
  val input: String =
    Source
      .fromResource(s"${self.getClass.getSimpleName}.txt")
      .getLines()
      .mkString("\n")
}

class Day01 extends AdventOfCodeSuite {
  val testInput = """
    |1000
    |2000
    |3000
    |
    |4000
    |
    |5000
    |6000
    |
    |7000
    |8000
    |9000
    |
    |10000
    |""".stripMargin.trim

  def parse(input: String): List[List[Int]] =
    input
      .split("\n")
      .foldLeft(List.empty[List[Int]]) {
        case (Nil, s)           => List(List(s.toInt))
        case (elves, "")        => Nil :: elves
        case (food :: elves, s) => (s.toInt :: food) :: elves
      }
      .map(_.reverse)
      .reverse

  def part1(s: String) = parse(s).map(_.sum).max

  def part2(s: String) = parse(s).map(_.sum).sorted.takeRight(3).sum

  test("part 1") {
    assert(part1(testInput) == 24000)
    println(part1(input))
  }

  test("part 2") {
    assert(part2(testInput) == 45000)
    println(part2(input))
  }
}

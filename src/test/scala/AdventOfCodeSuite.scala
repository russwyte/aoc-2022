import scala.io.Source

trait AdventOfCodeSuite extends munit.FunSuite { self =>
  val input: String =
    Source
      .fromResource(s"${self.getClass.getSimpleName}.txt")
      .getLines()
      .mkString("\n")
}
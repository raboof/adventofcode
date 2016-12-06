import scala.io.Source

object Main3 extends App {
  def parseLine(in: String) = in.trim.split("\\s+").map(_.toInt).toList
  def valid(sides: Seq[Int]) = sides.max < (sides.sum - sides.max)

  val first = Source.fromFile("input3.txt")
    .getLines
    .map(parseLine(_))
    .filter(valid(_))
    .size
  println(first)

  val second = Source.fromFile("input3.txt")
    .getLines
    .map(parseLine(_))
    .sliding(3, 3).map(_.toList)
    .flatMap(List.transpose(_))
    .filter(valid(_))
    .size

  println(second)
}

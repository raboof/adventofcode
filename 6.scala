import scala.io.Source

object Main6 extends App {
  val input1 = """eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar""".split('\n')

  val input2 = Source.fromFile("input6.txt").getLines.toList

  case class Histos(columns: Map[Int, Map[Char, Int]] = Map.empty) {
    def record(column: Int, char: Char) = Histos(
      columns.updated(column, {
        val map = columns.getOrElse(column, Map.empty)
        map.updated(char, map.get(char).getOrElse(0) + 1) })
    )
    lazy val cols: List[Map[Char, Int]] = columns.toList.sortBy(_._1).map(_._2)
  }

  @annotation.tailrec
  def getCode(input: Seq[String], histos: Histos = Histos(), sorter: (Int, Int) => Boolean = _ < _): String = {
    if (input.isEmpty) histos.cols.map(map => map.toList.sortWith((a, b) => sorter(a._2, b._2)).head._1).mkString
    else getCode(input.tail, input.head.zipWithIndex.foldLeft(histos) {
        case (acc, (char, idx)) => acc.record(idx, char)
      })
  }

  println(getCode(input2))
}

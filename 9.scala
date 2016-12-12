import scala.io.Source

object Main9 extends App {

  def uncompress(in: String) = {
      val MatchMarker = "\\((\\d+)x(\\d+)\\)(.*)".r
      def uncomp(in: String): String = in match {
        case "" => ""
        case MatchMarker(length, times, rest) =>
          rest.take(length.toInt) * times.toInt + uncomp(rest.drop(length.toInt))
        case other => other.head + uncomp(other.tail)
      }
      uncomp(in)
    }
    def uncompress2(in: String) = {
        val MatchMarker = "\\((\\d+)x(\\d+)\\)(.*)".r
        val NonMarker = "([^(]*)(.*)".r
        def uncomp(in: String): (Long, String) = in match {
          case "" => (0, "")
          case MatchMarker(length, times, rest) =>
            (uc(rest.take(length.toInt), 0) * times.toInt, rest.drop(length.toInt))
          case NonMarker(non, rest) => (non.length, rest)
        }
        @annotation.tailrec
        def uc(in: String, resultSoFar: Long): Long = uncomp(in) match {
          case (done, "") => resultSoFar + done
          case (done, rest) => uc(rest, resultSoFar + done)
        }
        uc(in, 0)
      }


    println(uncompress2("ADVENT"))
    println(uncompress2("A(1x5)BC"))
    println("x" + uncompress2("XYZ"))
    println(uncompress2("(3x3)XYZ"))
    println(uncompress2("A(2x2)BCD(2x2)EFG"))
    println(uncompress2("X(8x2)(3x3)ABCY"))

    println(uncompress(Source.fromFile("input9.txt").getLines.mkString).length)
    println(uncompress2(Source.fromFile("input9.txt").getLines.mkString))

  uncompress("ADVENT")
}

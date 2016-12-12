import scala.io.Source

object Main7 extends App {

  trait Section
  case object Hypernet extends Section
  case object Supernet extends Section

  @annotation.tailrec
  private def hasAbba(address: List[Char], found: Boolean = false, section: Section = Supernet): Boolean = address match {
    case '[' :: xs => hasAbba(xs, found, Hypernet)
    case ']' :: xs => hasAbba(xs, found, Supernet)
    case a :: b :: c :: d :: xs if a != b && a == d && b == c =>
           if (section == Hypernet) false
           else hasAbba(xs, found = true)
    case x :: xs => hasAbba(xs, found, section)
    case Nil => found
  }

  hasAbba("abba[mnop]qrst".toList)
  hasAbba("abcd[bddb]xyyx".toList)
  hasAbba("aaaa[qwer]tyui".toList)
  hasAbba("ioxxoj[asdfgh]zxcvbn".toList)

  println(Source.fromFile("input7.txt").getLines.flatMap(in => if (hasAbba(in.toList)) Some(()) else None).length)
  val results = Source.fromFile("input7renate.txt").getLines.flatMap(in => if (hasAbba(in.toList)) Some(in) else None)
  java.nio.file.Files.write(java.nio.file.Paths.get("/tmp/out.txt"), results.mkString("\n").getBytes("UTF-8"))
  println(results.length)

  private def hasAbaBab(address: String): Boolean = {
    @annotation.tailrec
    def hasAbaBab(address: List[Char], abaSoFar: List[(Char, Char)], babSoFar: List[(Char, Char)], section: Section): Boolean = {
      address match {
        case '[' :: xs => hasAbaBab(xs, abaSoFar, babSoFar, Hypernet)
        case ']' :: xs => hasAbaBab(xs, abaSoFar, babSoFar, Supernet)
        case a :: b :: c :: xs if a == c && a != b => section match {
          case Supernet =>
            if (babSoFar.contains((b, a))) true
            else hasAbaBab(b :: c :: xs, (a, b) :: abaSoFar, babSoFar, Supernet)
          case Hypernet =>
            if (abaSoFar.contains((b, a))) true
            else hasAbaBab(b :: c :: xs, abaSoFar, (a, b) :: babSoFar, Hypernet)
        }
        case _ :: xs => hasAbaBab(xs, abaSoFar, babSoFar, section)
        case Nil => false
      }
    }
    hasAbaBab(address.toList, List.empty, List.empty, Supernet)
  }

  hasAbaBab("aba[bab]xyz") // true
  hasAbaBab("xyx[xyx]xyx") // false
  hasAbaBab("aaa[kek]eke") // true
  hasAbaBab("zazbz[bzb]cdb") // true

  println(Source.fromFile("input7.txt").getLines.flatMap(in => if (hasAbaBab(in)) Some(()) else None).length)
}

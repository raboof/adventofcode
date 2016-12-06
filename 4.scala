import scala.io.Source

object Main4 extends App {
  val input = "aaaaa-bbb-z-y-x-123[abxyz]"

    val regex = "(.*)-(\\d+)\\[(\\w+)\\]".r

    def tupleValues(one: (Int, Char), other: (Int, Char)) =
      if (one._1 < other._1) false
      else if (one._1 > other._1) true
      else one._2 < other._2

    def getChecksum(encryptedName: String) =
      encryptedName.replaceAll("-", "").groupBy(identity).toList.map { case (char, values) => (values.size, char) }.sortWith(tupleValues).take(5).map(_._2).mkString

    def sector(input: String) = input match {
      case regex(name, sector, checksum) =>
        if (getChecksum(name) == checksum) Some((name, sector.toInt))
        else None
    }

    println(Source.fromFile("rooms.txt").getLines.flatMap(sector).map(_._2).sum)

    def shift(coded: String, shift: Int) = coded.map {
      case '-' => ' '
      case char => {
        (((char - 'a' + shift) % 26) + 'a').toChar
      }
    }

    println(shift("qzmt-zixmtkozy-ivhz", 343))

    println(Source.fromFile("rooms.txt").getLines.flatMap(sector).map{
      case (name, sector) => (shift(name, sector), sector)
    }.filter(_._1.contains("pole")).toList)
}

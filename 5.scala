object Main5 extends App {
  import java.security.MessageDigest

  val md5 = MessageDigest.getInstance("MD5")

  val doorId = "abc"

  @annotation.tailrec
  final def findKey(doorId: String, i: Int = 0, resultSoFar: List[Byte] = Nil): List[Byte] = {
    val digest = md5.digest(s"$doorId$i".getBytes)
    if (digest(0) == 0 && digest(1) == 0 && digest(2) >= 0 && digest(2) < 16) {
      if (resultSoFar.length == 7) resultSoFar :+ digest(2)
      else findKey(doorId, i+1, resultSoFar :+ digest(2))
    }
    else
      findKey(doorId, i+1, resultSoFar)
  }

    // findKey(doorId)

    def toHex(in: Seq[Byte]) = in.map(_.toInt.toHexString).mkString

    // cxdnnyjw
    println(toHex(findKey("cxdnnyjw")))

    val zero: Byte = 0
    def parseFirst4bits(b: Byte): Int = ((b & 0xFF) >>> 4)
    println(parseFirst4bits(-1))

    @annotation.tailrec
    final def findKey2(doorId: String, i: Int = 0, resultSoFar: List[Int] = List(-1, -1, -1, -1, -1, -1, -1, -1)): List[Int] = {
      val digest = md5.digest(s"$doorId$i".getBytes)
      if (digest(0) == 0 && digest(1) == 0 && digest(2) >= 0 && digest(2) < 8 && resultSoFar(digest(2)) == -1) {
        val updated = resultSoFar.updated(digest(2), parseFirst4bits(digest(3)))
        if (!updated.contains(-1)) updated
        else findKey2(doorId, i+1, updated)
      }
      else
        findKey2(doorId, i+1, resultSoFar)
    }
    def toHex2(in: List[Int]) = in.map(_.toHexString).mkString

    println(toHex2(findKey2("cxdnnyjw")))
}

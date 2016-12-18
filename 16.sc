object Main16 extends App {

  def invert(in: String): String = in.map {
    case '0' => '1'
    case '1' => '0'
  }

  @annotation.tailrec
  def  generate(a: String, desired: Int): String = {
    if (a.length >= desired) a.take(desired)
    else generate(a + "0" + invert(a.reverse), desired)
  }

  generate("1", 3)
  generate("0", 3)
  generate("11111", 11)
  generate("111100001010", 26)

  val data = generate("10011111011011001", 272)

  def checksum(data: String): String = {
    val compressed = data.sliding(2, 2).map {
      case "00" => '1'
      case "11" => '1'
      case "01" => '0'
      case "10" => '0'
    }.mkString
    if (compressed.size % 2 == 0) checksum(compressed)
    else compressed
  }
  checksum("110010110100")

  checksum(generate("10000", 20))
  checksum(generate("10011111011011001", 272))
  checksum(generate("10011111011011001", 35651584))
}

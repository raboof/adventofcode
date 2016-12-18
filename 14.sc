import java.security.MessageDigest

object Main14 extends App {
  val md5 = MessageDigest.getInstance("MD5")
  val salt = "jlmsuwbz"
  val salt = "abc"
  def hash(in: String): String = md5.digest(in.getBytes).map("%02x".format(_)).mkString
  def triplet(hash: Seq[Char]): Option[String] = hash match {
    case Seq(a, b, c, _ @ _*) if a == b && b == c => Some(a.toString)
    case Seq(_, rest @ _*) => triplet(rest)
    case Seq() => None
  }
  Stream
    .from(0)
    .map(n => hash(salt + n))
    .zipWithIndex
    .sliding(1000)
    .flatMap( set =>
      triplet(set.head._1)
        .filter(char => set.tail.exists(_._1.contains(char * 5)))
        .map(_ => set.head._2)
    )
    .drop(63).next()

    @annotation.tailrec
    def stretchedHash(in: String, timesLeft: Int = 2017): String = timesLeft match {
      case 0 => in
      case n => stretchedHash(hash(in), timesLeft - 1)
    }

    stretchedHash("abc0", 1)
    stretchedHash("abc0", 2)
    stretchedHash("abc0", 2017)
    stretchedHash("abc0")

    Stream
      .from(0)
      .map(n => (stretchedHash(salt + n), n))
      .sliding(1000)
      .flatMap( set =>
        triplet(set.head._1)
          .filter(char => set.tail.exists(_._1.contains(char * 5)))
          .map(_ => set.head._2)
      )
      .drop(63)
      .next()
}

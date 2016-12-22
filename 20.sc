import scala.io.Source
import java.math.BigInteger
object Main20 extends App {
    val ranges =
      Source.fromFile("20.txt")
        .getLines
        .map(_.split('-'))
        .toList
        .map {
          // Let's see if we get lucky without integers above (signed) maxint
          case Array(left, right) => (new BigInteger(left), new BigInteger(right))
        }
        .sortBy(_._1)

      val myAnswer = new BigInteger("15227950")
      ranges.filter{
        case (left, right) => left.compareTo(myAnswer) <= 0 && myAnswer.compareTo(right) <= 0
      }

      @annotation.tailrec
      def findFirstGap(current: BigInteger, ranges: List[(BigInteger, BigInteger)]): Option[BigInteger] = ranges match {
        case Nil => None
        case (from, to) :: xs =>
          if (current.compareTo(from) < 0) Some(current)
          else if (current.compareTo(to) > 0) findFirstGap(current, xs)
          else findFirstGap(to.add(BigInteger.ONE), xs)
      }
      findFirstGap(BigInteger.ZERO, ranges)

      @annotation.tailrec
      def numberOfIps(current: BigInteger, ranges: List[(BigInteger, BigInteger)], allowedSoFar: BigInteger): BigInteger = ranges match {
        case Nil => allowedSoFar.add(new BigInteger("2").pow(32).subtract(current))
        case (from, to) :: xs =>
          if (current.compareTo(from) < 0) numberOfIps(to.add(BigInteger.ONE), xs, allowedSoFar.add(from.subtract(current)))
          else if (current.compareTo(to) > 0) numberOfIps(current, xs, allowedSoFar)
          else numberOfIps(to.add(BigInteger.ONE), xs, allowedSoFar)
      }

      new BigInteger("2").pow(32)
      numberOfIps(BigInteger.ZERO, List(), BigInteger.ZERO)
      numberOfIps(BigInteger.ZERO, List((BigInteger.ZERO, BigInteger.ZERO), (BigInteger.ZERO, BigInteger.ZERO)), BigInteger.ZERO)
      numberOfIps(BigInteger.ZERO, List((BigInteger.ZERO, BigInteger.ONE)), BigInteger.ZERO)
      numberOfIps(BigInteger.ZERO, List((BigInteger.ONE, BigInteger.ONE)), BigInteger.ZERO)
      numberOfIps(BigInteger.ZERO, List((BigInteger.ZERO, BigInteger.ZERO),(BigInteger.ONE, BigInteger.ONE)), BigInteger.ZERO)
      val TWO = new BigInteger("2")
      numberOfIps(BigInteger.ZERO, List((BigInteger.ZERO, BigInteger.ZERO),(TWO, TWO)), BigInteger.ZERO)
      numberOfIps(BigInteger.ZERO, List((BigInteger.ZERO, TWO),(TWO, TWO)), BigInteger.ZERO)
      numberOfIps(BigInteger.ZERO, List((BigInteger.ZERO, BigInteger.ZERO),(BigInteger.ZERO, TWO)), BigInteger.ZERO)
      numberOfIps(BigInteger.ZERO, List((BigInteger.ZERO, TWO),(BigInteger.ZERO, BigInteger.ZERO)), BigInteger.ZERO)
      numberOfIps(BigInteger.ZERO, ranges, BigInteger.ZERO)
      // 893530153 was too high
}

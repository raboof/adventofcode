object Main19 extends App {
  val elves = Range(0, 3004953).toSet
  val numberOfElves = elves.size

  @annotation.tailrec
  def nextElf(current: Int, remaining: Set[Int]): Int =
    if (current == numberOfElves) nextElf(0, remaining)
    else if (remaining.contains(current)) current
    else nextElf(current + 1, remaining)

  @annotation.tailrec
  def findLastElf(current: Int, remaining: Set[Int]): Int = {
    val next = nextElf(current + 1, remaining)
    if (next == current) (next + 1)
    else {
      val rest = remaining - next
      findLastElf(nextElf(next, rest), rest)
    }
  }

  findLastElf(0, elves)

  def findLastElfB(numberOfElves: Int): Int = {
    import scala.collection.mutable.ArrayBuffer
    val remainingElves = new ArrayBuffer() ++ Range(0, numberOfElves)
    @annotation.tailrec
    def inner(currentIndex: Int, numberOfRemainingElves: Int): Int = numberOfRemainingElves match {
      case 1 => remainingElves(0) + 1
      case other =>
        val indexToBeStolenFrom = (currentIndex + (numberOfRemainingElves / 2)) % numberOfRemainingElves
        remainingElves.remove(indexToBeStolenFrom)
        val nextNumberOfRemainingElves = numberOfRemainingElves - 1
        val nextIndex = if (indexToBeStolenFrom < currentIndex) currentIndex else ((currentIndex + 1) % nextNumberOfRemainingElves)
        inner(nextIndex, nextNumberOfRemainingElves)
    }
    inner(0, numberOfElves)
  }
  findLastElfB(5)
  findLastElfB(3004953)
}

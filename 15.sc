object Main15 extends App {
  case class Disc(initialPosition: Int, numberOfPositions: Int)

  val input1txt = """Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1."""
  val input2txt = """Disc #1 has 17 positions; at time=0, it is at position 5.
Disc #2 has 19 positions; at time=0, it is at position 8.
Disc #3 has 7 positions; at time=0, it is at position 1.
Disc #4 has 13 positions; at time=0, it is at position 7.
Disc #5 has 5 positions; at time=0, it is at position 1.
Disc #6 has 3 positions; at time=0, it is at position 0."""

  val MatchDisc = "Disc #\\d+ has (\\d+) positions; at time=0, it is at position (\\d+).".r

  def parse(input: String): Seq[Disc] = input.split('\n').map {
    case MatchDisc(numberOfPositions, initialPosition) => Disc(initialPosition.toInt, numberOfPositions.toInt)
  }

  val input1 = parse(input1txt)

  def fallsThrough(discs: Seq[Disc], time: Int) =
    discs.zipWithIndex.forall {
      case (disc, idx) => ((disc.initialPosition + time + 1 + idx) % disc.numberOfPositions) == 0
    }

    Stream.from(1).filter(time => fallsThrough(input1, time)).take(1)

    Stream.from(1).filter(time => fallsThrough(parse(input2txt), time)).take(1)

    Stream.from(1).filter(time => fallsThrough(parse(input2txt) ++ Seq(Disc(0, 11)), time)).take(1)
}

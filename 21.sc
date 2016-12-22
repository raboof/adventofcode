import scala.io.Source

object Main21 extends App {
  val start = "abcde"
  val ops = List(
    "swap position 4 with position 0",
    "swap letter d with letter b",
    "reverse positions 0 through 4",
    "rotate left 1 step",
    "move position 1 to position 4",
    "move position 3 to position 0",
    "rotate based on position of letter b",
    "rotate based on position of letter d"
  )

  val SwapPositions = "swap position (\\d+) with position (\\d+)".r
  val SwapLetters = "swap letter (\\w+) with letter (\\w+)".r
  val Rotate = "rotate (\\w+) (\\d+) steps?".r
  val RotatePos = "rotate based on position of letter (\\w+)".r
  val ReversePositions = "reverse positions (\\d+) through (\\d+)".r
  val Move = "move position (\\d+) to position (\\d+)".r

  def rotateLeft(in: String, by: Int): String = {
    if (by < 0) rotateLeft(in, by + in.length)
    else if (by > in.length) rotateLeft(in, by - in.length)
    else in.drop(by) + in.take(by)
  }
  def rotateRight(in: String, by: Int): String = rotateLeft(in, in.length - by)


  def applyCommand(in: String, command: String): String = command match {
    case SwapPositions(x, y) => in.updated(x.toInt, in(y.toInt)).updated(y.toInt, in(x.toInt))
    case SwapLetters(x, y) => {
      val x0 = x(0)
      val y0 = y(0)
      in.map {
        case `x0` => y0
        case `y0` => x0
        case other => other
      }
    }
    case Rotate("right", n) => rotateRight(in, n.toInt)
    case Rotate("left", n) => rotateLeft(in, n.toInt)
    case RotatePos(x) => {
      val index = in.indexOf(x(0))
      val rotation = 1 + index + (if (index >= 4) 1 else 0)
      rotateRight(in, rotation)
    }
    case ReversePositions(from, to) =>
      in.take(from.toInt) + in.substring(from.toInt, to.toInt + 1).reverse + in.drop(to.toInt + 1)
    case Move(from, to) => {
      val rm = in.take(from.toInt) + in.drop(from.toInt + 1)
      rm.take(to.toInt) + in(from.toInt) + rm.drop(to.toInt)
    }
  }

  def rotateBack(in: String, by: Char): String = {
    @annotation.tailrec
    def inner(rotation: Int): String = {
      val orig = rotateLeft(in, rotation)
      // Hmm not sure if this can yield false positives... property-based testing would be nice here :)
      if (applyCommand(orig, s"rotate based on position of letter $by") == in) orig
      else inner(rotation + 1)
    }

    inner(1)
  }

  val rot = applyCommand("abcde", "rotate based on position of letter b")
  val back = rotateBack(rot, 'b')

  def unapplyCommand(in: String, command: String): String = command match {
    case SwapPositions(x, y) => in.updated(x.toInt, in(y.toInt)).updated(y.toInt, in(x.toInt))
    case SwapLetters(x, y) => {
      val x0 = x(0)
      val y0 = y(0)
      in.map {
        case `x0` => y0
        case `y0` => x0
        case other => other
      }
    }
    case Rotate("right", n) => rotateLeft(in, n.toInt)
    case Rotate("left", n) => rotateRight(in, n.toInt)
    case RotatePos(x) => rotateBack(in, x(0))
    case ReversePositions(from, to) =>
      in.take(from.toInt) + in.substring(from.toInt, to.toInt + 1).reverse + in.drop(to.toInt + 1)
    case Move(to, from) => {
      val rm = in.take(from.toInt) + in.drop(from.toInt + 1)
      rm.take(to.toInt) + in(from.toInt) + rm.drop(to.toInt)
    }
  }

  ops.foldLeft(start)((current, command) => applyCommand(current, command))

  Source.fromFile("21.txt").getLines.foldLeft("abcdefgh")((current, command) => applyCommand(current, command))

  Source.fromFile("21.txt").getLines.toList.reverse.foldLeft("fbgdceah")((current, command) => unapplyCommand(current, command))

  def roundtrip(command: String): Boolean =
    unapplyCommand(applyCommand("abcde", command), command) == "abcde"

  roundtrip("rotate based on position of letter b")

  applyCommand("abcde", "swap position 4 with position 0")
  applyCommand("ebcda", "swap letter d with letter b")
  applyCommand("edcba", "reverse positions 0 through 4")
  applyCommand("abcde", "rotate left 1 step")
  applyCommand("bcdea", "move position 1 to position 4")
  applyCommand("bdeac", "move position 3 to position 0")
  applyCommand("abdec", "rotate based on position of letter b")
  applyCommand("ecabd", "rotate based on position of letter d")
  val in = "ecabd"
  val index = in.indexOf('d')
  val rotation = 1 + index + (if (index >= 4) 1 else 0)
  in.drop((2 * in.length - rotation) % in.length) + in.take((2 * in.length - rotation) % rotation)

  applyCommand("start", "swap position 3 with position 1")
  applyCommand("start", "swap letter t with letter r")
  applyCommand("abcd", "rotate right 1 step")
  applyCommand("abcd", "rotate left 1 step")
  applyCommand("start", "rotate based on position of letter t")
  applyCommand("start", "reverse positions 0 through 3")
  applyCommand("start", "move position 2 to position 4")
}

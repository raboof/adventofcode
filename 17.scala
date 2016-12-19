object Main17 extends App {
  import java.security.MessageDigest
  val md5 = MessageDigest.getInstance("MD5")

  val up = 'U'
  val down = 'D'
  val left = 'L'
  val right = 'R'

  val passcode = "lpvhkcbi"
  case class State(x: Int, y: Int, pathSoFar: String) {
    lazy val hash = md5.digest((passcode + pathSoFar).getBytes).take(2).map("%02x".format(_)).mkString
    def next = List(go(left), go(right), go(up), go(down)).flatten
    def go(direction: Char): Option[State] = {
      val newCoords: (Int, Int) = direction match {
        case `up` => (x, y-1)
        case `down` => (x, y+1)
        case `left` => (x-1, y)
        case `right` => (x+1, y)
      }
      Some(newCoords)
        .filter { case (x, y) => x >= 0 && x < 4 && y >= 0 && y < 4 && canGo(direction) }
        .map { case (x, y) => State(x, y, pathSoFar + direction)}
    }
    def canGo(direction: Char): Boolean = "bcdef".contains(hash(direction match {
          case `up` => 0
          case `down` => 1
          case `left` => 2
          case `right` => 3
        }))
    def isAtTarget: Boolean = x == 3 && y == 3
  }



  @annotation.tailrec
  def shortestPath(currentStates: List[State], nextStates: List[State]): Option[String] = currentStates match {
    case Nil =>
      if (nextStates == Nil) None
      else shortestPath(nextStates, List.empty)
    case x :: _ if x.isAtTarget => Some(x.pathSoFar)
    case x :: xs => shortestPath(xs, x.next ++ nextStates)
  }

  shortestPath(List(State(0, 0, "")), List.empty)

  @annotation.tailrec
  def longestPath(currentStates: List[State], deepestSoFar: Option[Int]): Option[Int] = currentStates match {
    case Nil => deepestSoFar
    case states => {
      val deepest = currentStates.find(_.isAtTarget).map(_.pathSoFar.length).orElse(deepestSoFar)
      longestPath(currentStates.filter(!_.isAtTarget).flatMap(_.next), deepest)
    }
  }

  longestPath(List(State(0, 0, "")), None)

}

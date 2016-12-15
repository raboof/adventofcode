object Main13 extends App {
  sealed trait Space
  case object Open extends Space
  case object Wall extends Space
  case object Visited extends Space
  case object Target extends Space

  val magicNumber = 1350
  val width = 100
  val height = 100

  @annotation.tailrec
  def bitsEven(i: Int, soFar: Boolean): Boolean =
    if (i == 0) soFar
    else {
      val next = i >> 1
      if (i % 2 == 0) bitsEven(next, soFar)
      else bitsEven(next, !soFar)
    }

  val maze = Array.fill[Array[Space]](width)(
    Array.fill[Space](height)(Open)
  )

  /** careful: inverts X and Y */
  def mazeToString(maze: Array[Array[Space]]): String =
    maze.map(col => col.map {
      case Open => " "
      case Wall => "#"
      case Target => "T"
    }
      .mkString).mkString("\n")

  for (
    x <- Range(0, width);
    y <- Range(0, height)
  ) {
    maze(x)(y) = if (bitsEven(x*x + 3*x + 2*x*y + y + y*y + magicNumber, true)) Open else Wall
  }
  maze(31)(39) = Target
  mazeToString(maze)

  def locationsAround(x: Int, y: Int): Set[(Int, Int)] =
    Set((x-1, y), (x+1, y), (x, y-1), (x, y+1)).filter(_._1 >= 0).filter(_._2 >= 0)

  @annotation.tailrec
  def calculateSteps(currentDepth: Int, currentLevel: List[(Int, Int)], nextLevel: Set[(Int, Int)]): Int = currentLevel match {
    case Nil => calculateSteps(currentDepth + 1, nextLevel.toList, Set.empty)
    case (x, y) :: xys => maze(x)(y) match {
      case Target => currentDepth
      case Visited => calculateSteps(currentDepth, xys, nextLevel)
      case Wall => calculateSteps(currentDepth, xys, nextLevel)
      case Open => {
        maze(x)(y) = Visited
        calculateSteps(currentDepth, xys, nextLevel ++ locationsAround(x, y))
      }
    }
  }
  calculateSteps(0, List((1, 1)), Set.empty)

  @annotation.tailrec
  def calculateNumberOfVisitablePositionsWithin50(currentDepth: Int, currentLevel: List[(Int, Int)], nextLevel: Set[(Int, Int)]): Int = currentLevel match {
    case Nil =>
      if (currentDepth == 50) maze.flatten.count(_ == Visited)
      else calculateNumberOfVisitablePositionsWithin50(currentDepth + 1, nextLevel.toList, Set.empty)
    case (x, y) :: xys => maze(x)(y) match {
      case Target => ???
      case Visited => calculateNumberOfVisitablePositionsWithin50(currentDepth, xys, nextLevel)
      case Wall => calculateNumberOfVisitablePositionsWithin50(currentDepth, xys, nextLevel)
      case Open => {
        maze(x)(y) = Visited
        calculateNumberOfVisitablePositionsWithin50(currentDepth, xys, nextLevel ++ locationsAround(x, y))
      }
    }
  }
  calculateNumberOfVisitablePositionsWithin50(0, List((1, 1)), Set.empty)
}

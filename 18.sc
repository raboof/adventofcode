object Main18 extends App {
  val row = "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."
  def next(row: String) = ('.' +: row :+ '.')
    .sliding(3)
    .map {
      case "^^." => '^'
      case ".^^" => '^'
      case "..^" => '^'
      case "^.." => '^'
      case _ => '.'
    }
    .mkString

    next("..^^.")
    next(next("..^^."))

    @annotation.tailrec
    def countSafeTiles(currentRow: String, rowsToGo: Int, tilesSoFar: Int): Int = {
      if (rowsToGo == 0) tilesSoFar
      else countSafeTiles(next(currentRow), rowsToGo - 1, tilesSoFar + currentRow.count(_ == '.'))
    }

    countSafeTiles(row, 40, 0)
    countSafeTiles(row, 400000, 0)
}

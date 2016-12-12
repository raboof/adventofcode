sealed trait Isotope
case object Polonium extends Isotope
case object Thulium extends Isotope
case object Promethium extends Isotope
case object Ruthenium extends Isotope
case object Cobalt extends Isotope

case class Floor(generators: Set[Isotope] = Set.empty, chips: Set[Isotope] = Set.empty) {
  lazy val isValid = generators.isEmpty || chips.forall(generators.contains(_))
  lazy val isEmpty = generators.isEmpty && chips.isEmpty
}

case class Situation(
  currentLevel: Int,
  floors: List[Floor]
) {
  lazy val isValid = currentLevel >= 0 && currentLevel < floors.size && floors.forall(_.isValid)
  lazy val isDone = floors.init.forall(_.isEmpty)
}

val initialSituation = Situation(0, List(
  Floor(
    generators = Set(Polonium, Thulium, Promethium, Ruthenium, Cobalt),
    chips = Set(Thulium, Ruthenium, Cobalt)
  ),
  Floor(
    chips = Set(Polonium, Promethium)
  ),
  Floor(),
  Floor()
))

initialSituation.isValid

case class Move(direction: Int, generators: Set[Isotope], chips: Set[Isotope]) {
  def applyTo(situation: Situation): Situation =
    Situation(situation.currentLevel + direction,
      situation.floors.zipWithIndex.map {
        case (floor, idx) if idx == situation.currentLevel =>
          floor.copy(
            generators = floor.generators -- generators,
            chips = floor.chips -- chips
          )
        case (floor, idx) if idx == situation.currentLevel + direction =>
          floor.copy(
            generators = floor.generators ++ generators,
            chips = floor.chips ++ chips
          )
        case (floor, _) => floor
      }
    )
}
def allMoves(situation: Situation): Set[Move] = {
  val possiblePayloads: Set[(Set[Isotope], Set[Isotope])] = {
    val floor = situation.floors(situation.currentLevel)
    floor
      .generators
      .subsets
      .flatMap(selectedGenerators =>
        floor.chips
          .subsets
          .map(selectedChips => (selectedGenerators, selectedChips)))
      .filter(t => t._1.size + t._2.size > 0)
      .filter(t => t._1.size + t._2.size <= 4)
      .toSet
  }
  possiblePayloads.map { case (x, y) => Move(-1, x, y) } ++
  possiblePayloads.map { case (x, y) => Move(1, x, y) }
}

allMoves(initialSituation)
  .map(_.applyTo(initialSituation))
  .filter(_.isValid)

@annotations.tailrec
def numberOfMovesNeeded(seen: Set[Situation], now: Set[Situation], currentDepth: Int)

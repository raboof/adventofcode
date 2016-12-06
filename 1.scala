object One {
  
  sealed trait Rotation
  case object Left extends Rotation
  case object Right extends Rotation
  case object Straight extends Rotation
  object Rotation {
    def apply(in: Char) = in match {
      case 'L' => Left
      case 'R' => Right
    }
  }

  sealed trait Direction {
    def turn(rotation: Rotation): Direction = rotation match {
      case Left => this match {
        case North => West
        case West => South
        case South => East
        case East => North
      }
      case Right => this.turn(Left).turn(Left).turn(Left)
      case Straight => this
    } 
  }
  case object North extends Direction
  case object West extends Direction
  case object South extends Direction
  case object East extends Direction
  
  North.turn(Left)
  
  type Steps = Int
  object Steps {
    def apply(s: String) = s.toInt
  }

  def parse(in: String) = 
    in.split(", ").map(i => (Rotation(i.head), Steps(i.tail))).toList
  
  case class Location(x: Int, y: Int) {
    def step(d: Direction, s: Steps = 1): Location = d match {
      case North => Location(x, y + s)
      case East => Location(x + s, y)
      case South => Location(x, y - s)
      case West => Location(x - s, y)
    }
    lazy val distance = Math.abs(x) + Math.abs(y)
  }
  
  case class State(orientation: Direction, location: Location) {
    def follow(r: Rotation, n: Steps): State = {
      val newOrientation = orientation.turn(r)
      State(newOrientation, location.step(newOrientation, n))
    }
  }
  
  def walk(from: State, instructions: Seq[(Rotation, Steps)]): State = instructions.foldLeft(from) {
    case (s: State, (r: Rotation, n: Steps)) => s.follow(r, n)
  }
  
  // val instructions = walk(parse("R5, L5, R5, R3"))
  val instructions = "L3, R1, L4, L1, L2, R4, L3, L3, R2, R3, L5, R1, R3, L4, L1, L2, R2, R1, L4, L4, R2, L5, R3, R2, R1, L1, L2, R2, R2, L1, L1, R2, R1, L3, L5, R4, L3, R3, R3, L5, L190, L4, R4, R51, L4, R5, R5, R2, L1, L3, R1, R4, L3, R1, R3, L5, L4, R2, R5, R2, L1, L5, L1, L1, R78, L3, R2, L3, R5, L2, R2, R4, L1, L4, R1, R185, R3, L4, L1, L1, L3, R4, L4, L1, R5, L5, L1, R5, L1, R2, L5, L2, R4, R3, L2, R3, R1, L3, L5, L4, R3, L2, L4, L5, L4, R1, L1, R5, L2, R4, R2, R3, L1, L1, L4, L3, R4, L3, L5, R2, L5, L1, L1, R2, R3, L5, L3, L2, L1, L4, R4, R4, L2, R3, R1, L2, R1, L2, L2, R3, R3, L1, R4, L5, L3, R4, R4, R1, L2, L5, L3, R1, R4, L2, R5, R4, R2, L5, L3, R4, R1, L1, R5, L3, R1, R5, L2, R1, L5, L2, R2, L2, L3, R3, R3, R1"
  val endState = walk(State(North, Location(0,0)), parse(instructions))
  endState.location.distance 

  @annotation.tailrec
  private def firstSeenTwice(in: Seq[(Rotation, Steps)], d: Direction = North, seen: List[Location] = List(Location(0,0))): Option[Location] = in match {
    case Nil => None
    case (_, 0) :: rest => firstSeenTwice(rest, d, seen)
    case (Straight, n) :: rest => {
      val nextLocation = seen.head.step(d)
      if (seen.contains(nextLocation)) Some(nextLocation)
      else firstSeenTwice((Straight, n-1) :: rest, d, nextLocation :: seen)
    }
    case (r, steps) :: rest => firstSeenTwice((Straight, steps) :: rest, d.turn(r), seen)  
  }
  
  val fst = firstSeenTwice(parse(instructions))
  fst.map(_.distance)
}

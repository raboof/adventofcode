import scala.io.Source

sealed trait Node
case class Input(i: Int) extends Node
case class Output(o: Int) extends Node
case class Bot(i: Int) extends Node

sealed trait Command
case class Move(value: Int, destination: Node) extends Command
case class Rule(from: Int, lowTo: Node, highTo: Node) extends Command
// case class Move(value: Int, target: Node) extends Command

val MatchMove = "value (\\d+) goes to (\\w+ \\d+)".r
val MatchRule = "bot (\\d+) gives low to (\\w+ \\d+) and high to (\\w+ \\d+)".r
val MatchBot = "bot (\\d+)".r
val MatchOutput = "output (\\d+)".r

def parseNode(in: String) = in match {
  case MatchBot(in) => Bot(in.toInt)
  case MatchOutput(in) => Output(in.toInt)
}

def parseCommand(line: String): Command = line match {
  case MatchMove(v, b) => Move(v.toInt, parseNode(b))
  case MatchRule(from, low, high) => Rule(from.toInt, parseNode(low), parseNode(high))
}

// val commands = """value 5 goes to bot 2
// bot 2 gives low to bot 1 and high to bot 0
// value 3 goes to bot 1
// bot 1 gives low to output 1 and high to bot 0
// bot 0 gives low to output 2 and high to output 0
// value 2 goes to bot 2""".split("\n").map(parseCommand)

val commands = Source.fromFile("input10.txt").getLines.map(parseCommand).toList
val rules = commands.collect { case rule: Rule => (rule.from -> rule) }.toMap

@annotation.tailrec
def interpret(commands: List[Move], botsWithOneToken: Map[Int, Int]): Int = commands match {
  case Move(value, Bot(botId)) :: more => {
    botsWithOneToken.get(botId) match {
        case Some(existingToken) => {
          val min = Math.min(existingToken, value)
          val max = Math.max(existingToken, value)
          if (min == 17 && max == 61) botId
          else interpret(Move(min, rules(botId).lowTo) :: Move(max, rules(botId).highTo) :: more, botsWithOneToken - botId)
        }
        case None =>
          interpret(more, botsWithOneToken.updated(botId, value))
    }
  }
  case _ :: more => interpret(more, botsWithOneToken)
}

val moves = commands.collect { case move: Move => move }.toList
interpret(moves, Map.empty)

@annotation.tailrec
def interpret2(commands: List[Move], botsWithOneToken: Map[Int, Int], outputs: Map[Int, Int]): Int = commands match {
  case Move(value, Output(outputId)) :: more => {
      if (outputId == 0 || outputId == 1 || outputId == 2) {
        val newOutputs = outputs.updated(outputId, value)
        (newOutputs.get(0), newOutputs.get(1), newOutputs.get(2)) match {
          case (Some(one), Some(two), Some(three)) => one * two * three
          case other => interpret2(more, botsWithOneToken, newOutputs)
        }
      } else interpret2(more, botsWithOneToken, outputs)
  }
  case Move(value, Bot(botId)) :: more => {
    botsWithOneToken.get(botId) match {
        case Some(existingToken) => {
          val min = Math.min(existingToken, value)
          val max = Math.max(existingToken, value)
          interpret2(Move(min, rules(botId).lowTo) :: Move(max, rules(botId).highTo) :: more, botsWithOneToken - botId, outputs)
        }
        case None =>
          interpret2(more, botsWithOneToken.updated(botId, value), outputs)
    }
  }
  case _ :: more => interpret(more, botsWithOneToken)
}

interpret2(moves, Map.empty, Map.empty)

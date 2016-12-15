object Main12 extends App {
  val program = """cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 19 c
cpy 11 d
inc a
dec d
jnz d -2
dec c
jnz c -5""".split("\n")
  val Instruction1 = "(\\w+) ([a-z0-9]+)".r
  val Instruction2 = "(\\w+) ([a-z0-9]+) (-?\\w+)".r

  def intValue(state: Map[String, Int], value: String): Int = state.get(value).getOrElse(value.toInt)

  @annotation.tailrec
  def evaluate(program: Seq[String], pc: Int = 0, state: Map[String, Int]): Map[String, Int] = {
    if (pc >= program.length) state
    else program(pc) match {
      case Instruction1("dec", register) => evaluate(program, pc + 1, state.updated(register, state(register) - 1))
      case Instruction1("inc", register) => evaluate(program, pc + 1, state.updated(register, state(register) + 1))
      case Instruction2("cpy", value, to) => evaluate(program, pc + 1, state.updated(to, intValue(state, value)))
      case Instruction2("jnz", a, move) => {
        val nextPc = if (intValue(state, a) == 0) pc + 1 else pc + intValue(state, move)
        evaluate(program, nextPc, state)
      }
    }
  }
  evaluate(program, pc = 0, state = List("a", "b", "c", "d").map(_ -> 0).toMap)
  evaluate(program, pc = 0, state = List("a", "b", "c", "d").map(_ -> 0).toMap.updated("c", 1))
}

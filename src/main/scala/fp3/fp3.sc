def fib(n: Int): Int = {
  @annotation.tailrec
  def go(cnt: Int, prev: Int, cur: Int): Int = cnt match {
    case m if (m < 0) => sys.error("Negative Number Not Allowed!")
    case 0 => prev
    case c => go(cnt - 1, cur, prev + cur)
  }

  go(n, 0, 1)
}
fib(5)
// go(5, 0, 1)
// go(4, 1, 1)
// go(3, 1, 2)
// go(2, 2, 3)
// go(1, 3, 5)
// go(0, 5, 8)


def formatResult(name: String, n: Int, f: Int => Int) = {
  val msg = "The %s of %d is %d."
  msg.format(name, n, f(n))
}

println(formatResult("factorial", 7, fib))
println(formatResult("increment", 7, (x: Int) => x + 1))
println(formatResult("increment2", 7, (x) => x + 1))
println(formatResult("increment3", 7, x => x + 1))
println(formatResult("increment4", 7, _ + 1))
println(formatResult("increment5", 7, x => {
  val r = x + 1; r
}))


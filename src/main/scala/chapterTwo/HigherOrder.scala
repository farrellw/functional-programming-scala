package chapterTwo

object HigherOrder {
  def abs(n: Int): Int = {
    if(n < 0) -n
    else n
  }

  def formatHigher(x: Int, name: String,f: Int => Int): String = {
   val msg = "The %s of %d is %d"
    msg.format(x, name, f(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(count: Int, acc: Int): Int = {
      if (count <= 1) acc
      else go(count - 1, acc * count)
    }
    go(n, 1)
  }

  def fib(n: Int): Int = {
    if (n <= 1) 0
    else if (n == 2 || n == 3) 1
    else fib(n - 1) + fib(n - 2)
  }

  def tailFib(n: Int): Int = {
    @annotation.tailrec
    def go(count: Int, prev: Int, cur: Int): Int = {
      if (count >= n) cur
      else go(count + 1, cur, prev + cur)
    }

    go(2, 0, 1)
  }
}

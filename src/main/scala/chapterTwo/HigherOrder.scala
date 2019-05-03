package chapterTwo

object HigherOrder {
  def fib(n: Int): Int = {
    if (n <= 1) 0
    else if (n == 2 || n == 3) 1
    else fib(n - 1) + fib(n - 2)
  }

  def tailFib(n: Int): Int = {
    def go(count: Int, prev: Int, cur: Int): Int = {
      if (count >= n) cur
      else go(count + 1, cur, prev + cur)
    }

    go(2, 0, 1)
  }
}

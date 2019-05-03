package chapterTwo

object HigherOrder {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def formatResult(x: Int, name: String, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, x, f(x))
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

  def isSorted[A](array: Array[A], f: (A, A) => Boolean): Boolean = {
    def go(a: Array[A], pos1: Int, pos2: Int): Boolean = {
      if (a.length <= pos2) true
      else {
        val sorted: Boolean = f(a(pos1), a(pos2))
        if (sorted) go(a, pos2, pos2 + 1)
        else return false
      }
    }

    go(array, 0, 1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }
}

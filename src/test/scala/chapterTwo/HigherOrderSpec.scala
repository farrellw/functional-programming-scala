package chapterTwo

import org.scalatest.FunSpec

class HigherOrderSpec extends FunSpec {
  describe("Fibonacci") {
    it("Finds nth number") {
      //The 15th number in the fibonacci sequence is 377
      val expected = 377
      val actual = HigherOrder.fib(15)
      assert(expected == actual)
    }
    it("Finds nth number using local tail-recursive function") {
      //The sixth number in the fibonacci sequence is 5
      val expected = 377
      val actual = HigherOrder.tailFib(15)
      assert(expected == actual)
    }
  }
  describe("Factorial") {
    it("Finds the nth number") {
      //The 4th number factorial is 24
      val expected = 24
      val actual = HigherOrder.factorial(4)
      assert(expected == actual)
    }
  }
  describe("Format Result") {
    it("Displays correct string") {
      val expected = "The absolute value of -42 is 42."
      val actual = HigherOrder.formatResult(-42, "absolute value", HigherOrder.abs)
      assert(expected == actual)
    }
  }
  describe("Is sorted") {
    it("Returns false when an Array is not sorted") {
      val expected = false
      val unsortedList = Array(1, 3, 2)

      def comparisonFunction(x: Int, y: Int): Boolean = {
        x <= y
      }

      val actual = HigherOrder.isSorted(unsortedList, comparisonFunction)
      assert(expected == actual)
    }

    it("Returns true when an Array is sorted") {
      val expected = true
      val unsortedList = Array(1, 2, 3)

      def comparisonFunction(x: Int, y: Int): Boolean = {
        x <= y
      }

      val actual = HigherOrder.isSorted(unsortedList, comparisonFunction)
      assert(expected == actual)
    }
  }
  describe("Curry") {
    def f(x: String, y: Int): (String, Int) = (x, y)

    it("Curried function executes the same as original function") {
      val curriedFunction = HigherOrder.curry(f)
      val expected = f("a", 1)
      val actual = curriedFunction("a")(1)
      assert(expected == actual)
    }
  }
  describe("Uncurry") {
    def f(x: String): Int => (String, Int) = {
      y: Int => (x, y)
    }

    it("Uncurried function executes the same as original function") {
      val uncurriedFunction = HigherOrder.uncurry(f)
      val expected = f("a")(1)
      val actual = uncurriedFunction("a", 1)
      assert(expected == actual)
    }
  }

  describe("Compose"){
    def f(x: String): String = x.capitalize
    def g(x: String): String = x.toLowerCase
    val originalString: String = "sCAla"

    it("Composed function executes the two functions given"){
      val composed: String => String = HigherOrder.compose(f, g)
      val expected: String = "Scala"
      val actual = composed(originalString)
      assert(expected == actual)
    }
    it("Composed function returns the same result as executing each function individually"){
      val composed: String => String = HigherOrder.compose(f, g)
      val expected = f(g(originalString))
      val actual = composed(originalString)
      assert(expected == actual)
    }
  }
}

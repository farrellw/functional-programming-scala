package chapterTwo

import org.scalatest.FunSpec

class HigherOrderSpec extends FunSpec{
  describe("Fibonacci"){
    it("Finds nth number"){
      //The 15th number in the fibonacci sequence is 377
      val expected = 377
      val actual = HigherOrder.fib(15)
      assert(expected == actual)
    }
    it("Finds nth number using local tail-recursive function"){
      //The sixth number in the fibonacci sequence is 5
      val expected = 377
      val actual = HigherOrder.tailFib(15)
      assert(expected == actual)
    }
  }
  describe("Factorial"){
    it("Finds the nth number"){
      //The 4th number factorial is 24
      val expected = 24
      val actual = HigherOrder.factorial(4)
      assert(expected == actual)
    }
  }
}

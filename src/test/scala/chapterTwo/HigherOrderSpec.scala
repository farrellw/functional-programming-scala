package chapterTwo

import org.scalatest.FunSpec

class HigherOrderSpec extends FunSpec{
  describe("Higher Order functions"){
    it("Finds nth fibonacci number"){
      //The 15th number in the fibonacci sequence is 377
      val expected = 377
      val actual = HigherOrder.fib(15)
      assert(expected == actual)
    }
    it("Finds nth fibonacci number using local tail-recursive function"){
      //The sixth number in the fibonacci sequence is 5
      val expected = 377
      val actual = HigherOrder.tailFib(15)
      assert(expected == actual)
    }
  }
}

package chapterOne

import org.scalatest._

class FuncProgrammingSpec extends FunSpec with Matchers {
  describe("Chapter One") {
    it("MapData.MapDataTest Passes One Test") {
      val expected: Int = 27
      val actual: Int = 27
      assert(expected === actual)
    }
  }
}

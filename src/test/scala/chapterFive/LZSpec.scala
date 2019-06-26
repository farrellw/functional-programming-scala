package chapterFive

import org.scalatest.FunSpec

class LZSpec extends FunSpec {
  describe("To List") {
    it("Converts a stream to a list") {
      val stream = LZ.Cons(() => 1, () => LZ.Cons(() => 2, () => LZ.Cons(() => 1 + 2, () => LZ.Empty)))
      val actual = stream.toList
      val expected = List(1, 2, 3)

      assert(actual == expected)
    }
  }

  describe("Take") {
    it("Takes first n elements of a stream") {
      val stream = LZ.Cons(() => 1, () => LZ.Cons(() => 2, () => LZ.Cons(() => 1 + 2, () => LZ.Empty)))
      val actual = stream.take(2)
      val expected = LZ.Cons(() => 1, () => LZ.Cons(() => 2, () => LZ.Empty))

      assert(actual.toList == expected.toList)
    }
  }

  describe("Drop") {
    it("Drops first n elements of a stream") {
      val stream = LZ.Cons(() => 1, () => LZ.Cons(() => 2, () => LZ.Cons(() => 1 + 2, () => LZ.Empty)))
      val actual = stream.drop(2)
      val expected = LZ.Cons(() => 1 + 2, () => LZ.Empty)

      assert(actual.toList == expected.toList)
    }
  }

  describe("TakeWhile") {
    it("Takes the first n elements of a stream that match a certain predicate") {
      def predicate(n: Int): Boolean = n < 3

      val stream = LZ.Cons(() => 1, () => LZ.Cons(() => 2, () => LZ.Cons(() => 1 + 2, () => LZ.Empty)))
      val actual = stream.takeWhile(predicate)
      val expected = LZ.Cons(() => 1, () => LZ.Cons(() => 2, () => LZ.Empty))

      assert(actual.toList == expected.toList)
    }
  }

  describe("For all") {
    def lessThan(y: Int)(x: Int): Boolean = {
      x < y
    }

    it("Checks that all elements in the Stream match a given predicate") {
      val stream = LZ.Cons(() => 1, () => LZ.Cons(() => 2, () => LZ.Cons(() => 1 + 2, () => LZ.Empty)))
      val actual = stream.forAll(lessThan(4))
      val expected = true

      assert(actual == expected)
    }

    it("Terminates traversal as soon as it encounters a nonmatching value") {
      val stream = LZ.Cons(() => 1, () => LZ.Cons(() => 2, () => LZ.Cons(() => 1 + 2, () => LZ.Empty)))
      val actual = stream.forAll(lessThan(2))
      val expected = false

      assert(actual == expected)
    }
  }
}

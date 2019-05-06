package chapterThree

import org.scalatest.FunSpec

class DSSpec extends FunSpec {
  describe("3.1") {
    val x = DS.List(1, 2, 3, 4, 5) match {
      case DS.Cons(x, DS.Cons(2, DS.Cons(4, _))) => x
      case DS.Nil => 42
      case DS.Cons(x, DS.Cons(y, DS.Cons(3, DS.Cons(4, _)))) => x + y
      case DS.Cons(h, t) => h + DS.List.sum(t)
      case _ => 101
    }

    it("Correctly matches the third case statement") {
      assert(x == 3)
    }
  }

  describe("Tail, 3.2") {
    it("Removes the first element from a list and returns the remaining list") {
      val actual = DS.List.tail(DS.List(1, 2, 3, 4))
      val expected = DS.List(2, 3, 4)

      assert(actual == expected)
    }

    it("Returns nil as the remaining list if the input is empty") {
      val actual = DS.List.tail(DS.Nil)
      val expected = DS.Nil
      assert(actual == expected)
    }
  }

  describe("SetHead 3.3") {
    it("Replaces the first element of list with a different value") {
      val newValue = 5
      val actual = DS.List.setHead(newValue, DS.List(1, 2, 3, 4))
      val expected = DS.List(5, 2, 3, 4)
      assert(actual == expected)
    }

    it("Returns the first element as the head even if nil is passed in") {
      val newValue = 5
      val actual = DS.List.setHead(newValue, DS.Nil)
      val expected = DS.List(5)
      assert(actual == expected)
    }
  }

  describe("Drop 3.4") {
    it("Removes the first n elements from a list") {
      val actual = DS.List.drop(2, DS.List(1, 2, 3, 4))
      val expected = DS.List(3, 4)
      assert(actual == expected)
    }

    it("Returns nil if more elements are requested to drop than exist ") {
      val actual = DS.List.drop(5, DS.List(1, 2, 3, 4))
      val expected = DS.Nil
      assert(actual == expected)
    }
  }

  describe("Drop While 3.5"){
    it("Removes elements from the List prefix as long as they match a predicate"){
      val actual = DS.List.dropWhile((a: Int) => a <= 2, DS.List(1,2,3,4))
      val expected = DS.List(3,4)
      assert(actual == expected)
    }

    it("Returns nil when all elements in the list match a predicate"){
      val actual = DS.List.dropWhile((a: Int) => a <= 5, DS.List(1,2,3,4))
      val expected = DS.Nil
      assert(actual == expected)
    }

    it("Returns nil when input list is nil"){
      val actual = DS.List.dropWhile((a: Int) => a <= 2, DS.Nil)
      val expected = DS.Nil
      assert(actual == expected)
    }
  }
}

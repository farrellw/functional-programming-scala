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

  describe("SetHead, 3.3") {
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

  describe("Drop, 3.4") {
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

  describe("Drop While, 3.5") {
    it("Removes elements from the List prefix as long as they match a predicate") {
      val actual = DS.List.dropWhile(DS.List(1, 2, 3, 4))(a => a <= 2)
      val expected = DS.List(3, 4)
      assert(actual == expected)
    }

    it("Returns nil when all elements in the list match a predicate") {
      val actual = DS.List.dropWhile(DS.List(1, 2, 3, 4))(a => a <= 5)
      val expected = DS.Nil
      assert(actual == expected)
    }

    it("Returns nil when input list is nil") {
      val actual = DS.List.dropWhile(DS.Nil)(a => false)
      val expected = DS.Nil
      assert(actual == expected)
    }
  }

  describe("Init, 3.6") {
    it("Returns a list consisting of all but the last element of a List") {
      val actual = DS.List.init(DS.List(1, 2, 3, 4))
      val expected = DS.List(1, 2, 3)

      assert(actual == expected)
    }
    it("Returns nil when the list input only has one element") {
      val actual = DS.List.init(DS.List(1))
      val expected = DS.Nil

      assert(actual == expected)
    }
    it("Returns nil when the input list is nil") {
      val actual = DS.List.init(DS.Nil)
      val expected = DS.Nil

      assert(actual == expected)
    }
  }

  describe("Length, 3.9") {
    it("Calculates length of a list using foldRight") {
      val actual = DS.List.length(DS.List(1, 2, 3, 4))
      val expected = 4

      assert(actual == expected)
    }
  }

  describe("Sum, product, length using FoldLeft, 3.11") {
    it("Calculates sum of a list using foldLeft") {
      val actual = DS.List.sumFoldLeft(DS.List(1, 2, 3, 4))
      val expected = 10

      assert(actual == expected)
    }

    it("Calculates product of a list using foldLeft") {
      val actual = DS.List.productFoldLeft(DS.List(1, 2, 3, 4))
      val expected = 24

      assert(actual == expected)
    }

    it("Calculates length of a list using foldLeft") {
      val actual = DS.List.lengthFoldLeft(DS.List(1, 2, 3, 4))
      val expected = 4

      assert(actual == expected)
    }
  }

  describe("Reverse, 3.12") {
    it("Returns reverse of a list") {
      val actual = DS.List.reverse(DS.List(1, 2, 3, 4))
      val expected = DS.List(4, 3, 2, 1)

      assert(actual == expected)
    }
  }

  describe("Append, 3.14") {
    it("Append in terms of foldRight") {
      val actual = DS.List.appendFoldRight(DS.List(1, 2, 3, 4), DS.List(5, 6))
      val expected = DS.List(1, 2, 3, 4, 5, 6)

      assert(actual == expected)
    }
  }

  describe("Map, 3.16-3.18") {
    it("Transforms a list of integers by adding 1 to each element") {
      val actual = DS.List.map(DS.List(1, 2, 3, 4))(_ + 1)
      val expected = DS.List(2, 3, 4, 5)

      assert(actual == expected)
    }

    it("Turns each value in a list[Double] to a String") {
      val actual = DS.List.map(DS.List(1.0, 2.0, 3.0))(_.toString)
      val expected = DS.List("1.0", "2.0", "3.0")

      assert(actual == expected)
    }
  }

  describe("Filter, 3.19") {
    it("Removes elements from a list unless they satisfy a given predicate.") {
      def isEven(a: Int): Boolean = {
        a % 2 == 0
      }

      val actual = DS.List.filter(DS.List(1, 2, 3, 4))(isEven)
      val expected = DS.List(2, 4)

      assert(actual == expected)
    }
  }

  describe("FlatMap, 3.2") {
    it("Works like a map except that the function given will return a list instead of a single result") {
      def newList(i: Int): DS.List[Int] = {
        DS.List(i, i)
      }

      val actual = DS.List.flatMap(DS.List(1, 2, 3))(newList)
      val expected = DS.List(1, 1, 2, 2, 3, 3)

      assert(actual == expected)
    }
  }

  describe("ZipWith, 3.23") {
    it("Zips two lists together according to a passed in function") {
      val actual = DS.List.zipWith(DS.List(1, 2, 3), DS.List(4, 5, 6))((x, y) => x + y)
      val expected = DS.List(5, 7, 9)

      assert(actual == expected)
    }
  }

  describe("Take, extra-credit") {
    it("Returns the first n elements of a list") {
      val actual = DS.List.take(DS.List(1, 2, 3), 2)
      val expected = DS.List(1, 2)

      assert(actual == expected)
    }
  }

  describe("Take-while, extra-credit") {
    it("Returns the first n elements that match a given function") {
      val actual = DS.List.takeWhile(DS.List(1, 2, 3))(_ < 2)
      val expected = DS.List(1)

      assert(actual == expected)
    }
  }

  describe("Forall, extra-credit"){
    it("Returns true if all elements of list match a predicate") {
      val actual = DS.List.forall(DS.List(1, 2, 3))(_ < 4)
      val expected = true

      assert(actual == expected)
    }

    it("Returns false if all elements of list match a predicate") {
      val actual = DS.List.forall(DS.List(1, 2, 3))(_ < 2)
      val expected = false

      assert(actual == expected)
    }
  }
}

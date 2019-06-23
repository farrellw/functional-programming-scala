package chapterFour

import org.scalatest.FunSpec

class EHSpec extends FunSpec {
  describe("Mean returns an option") {
    it("Returns a Some of an average of a Sequence with more than 0 values") {
      val mySeq = Seq(1.0, 3.0, 5.0)
      val actual = EH.mean(mySeq)
      val expected = EH.Some(3.0)
      assert(actual == expected)

    }
    it("Returns None if any empty sequence is passed") {
      val mySeq = Seq()
      val actual = EH.mean(mySeq)
      val expected = EH.None
      assert(actual == expected)
    }
  }

  describe("Map") {
    def timesTwo(x: Int): Int = x * 2

    it("Applies F when the option is not None") {
      val initialOption = EH.Some(2)

      val actual = initialOption.map(timesTwo)
      val expected = EH.Some(4)

      assert(actual == expected)
    }
    it("Returns None if the option is None") {
      val initialOption = EH.None

      val actual = initialOption.map(timesTwo)
      val expected = EH.None

      assert(actual == expected)
    }
  }

  describe("Flat Map") {
    it("Applies F as an option one layer deep") {
      val initialOption = EH.Some(Seq(1.0, 3.0, 5.0))
      val actual = initialOption.flatMap(EH.mean)
      val expected = EH.Some(3.0)

      assert(actual == expected)
    }
    it("Returns None if the initial option is none or the application of F is none") {
      val initialOption = EH.None
      val actual = initialOption.flatMap(EH.mean)
      val expected = EH.None

      assert(actual == expected)
    }
    it("Returns None if the application of F is none") {
      val initialOption = EH.Some(Seq())
      val actual = initialOption.flatMap(EH.mean)
      val expected = EH.None

      assert(actual == expected)
    }
  }

  describe("Get or Else") {
    it("Returns default value provided if the Option that is None") {
      val initialOption = EH.None
      val actual = initialOption.getOrElse(3)

      val expected = 3
      assert(actual == expected)
    }
    it("Returns the value within a Some") {
      val initialOption = EH.Some(2)
      val actual = initialOption.getOrElse(3)

      val expected = 2
      assert(actual == expected)
    }
  }

  describe("Or else") {
    it("Returns default value provided if Option is None") {
      val initialOption = EH.None
      val actual = initialOption.orElse(EH.Some(2))
      val expected = EH.Some(2)

      assert(actual == expected)
    }
    it("Returns the some(value) if it does exist in a some") {
      val initialOption = EH.Some(3)
      val actual = initialOption.orElse(EH.Some(2))
      val expected = EH.Some(3)

      assert(actual == expected)
    }
  }

  describe("Filter") {
    def isEven(a: Int): Boolean = {
      a % 2 == 0
    }

    it("Returns a Some if the Option provided has a value, and if function of that value is true.") {
      val initialOption = EH.Some(4)
      val actual = initialOption.filter(isEven)
      val expected = EH.Some(4)

      assert(actual == expected)
    }
    it("Returns None if the Option provided is None") {
      val initialOption = EH.None
      val actual = initialOption.filter(isEven)
      val expected = EH.None

      assert(actual == expected)
    }
    it("Returns None if the Option provided is Some, and the function of that value is false.") {
      val initialOption = EH.Some(3)
      val actual = initialOption.filter(isEven)
      val expected = EH.None

      assert(actual == expected)
    }
  }

  describe("Variance") {
    it("Returns the variance of Some()") {
      val actual = EH.variance(Seq(1.0, 3.0, 5.0))
      val expected = EH.Some(2.6666666666666665)
      assert(actual == expected)
    }
    it("Returns none when the mean is none") {
      val actual = EH.variance(Seq())
      val expected = EH.None
      assert(actual == expected)
    }
  }

  describe("Sequence") {
    it("Returns none if one of the values is none") {
      val beginningSequence = List(EH.Some(1), EH.None, EH.Some(3))
      val actual = EH.sequence(beginningSequence)
      val expected = EH.None

      assert(actual == expected)
    }
    it("Returns some of a list if all values are somes") {
      val beginningSequence = List(EH.Some(1), EH.Some(2), EH.Some(3))
      val actual = EH.sequence(beginningSequence)
      val expected = EH.Some(List(1, 2, 3))

      assert(actual == expected)
    }
  }

  describe("Traverse") {
    def timesTwo(x: Int): EH.Option[Int] = {
      if (x > 2) {
        EH.Some(x * 2)
      } else {
        EH.None
      }
    }

    it("Returns none if one of values returns none from the function passed in") {
      val beginningSequence = List(1, 2, 3)
      val actual = EH.traverse(beginningSequence)(timesTwo)
      val expected = EH.None

      assert(actual == expected)
    }
    it("Returns some of a list if all values return some from passed in.") {
      val beginningSequence = List(3, 4, 5)
      val actual = EH.traverse(beginningSequence)(timesTwo)
      val expected = EH.Some(List(6, 8, 10))

      assert(actual == expected)
    }
  }

  describe("Sequence in terms of traverse") {
    it("Returns none if one of the values is none") {
      val beginningSequence = List(EH.Some(1), EH.None, EH.Some(3))
      val actual = EH.sequenceInTermsOfTraverse(beginningSequence)
      val expected = EH.None

      assert(actual == expected)
    }
    it("Returns some of a list if all values are somes") {
      val beginningSequence = List(EH.Some(1), EH.Some(2), EH.Some(3))
      val actual = EH.sequenceInTermsOfTraverse(beginningSequence)
      val expected = EH.Some(List(1, 2, 3))

      assert(actual == expected)
    }
  }

  describe("Map") {
    def timesTwo(i: Int): Int = i * 2

    it("Applies the function when the option is right") {
      val actual = EH.Right(2).map(timesTwo)
      val expected = EH.Right(4)

      assert(actual == expected)
    }
    it("Returns the left when the option is left") {
      val exception = new Exception("An Error Occurred")
      val actual = EH.Left(exception).map(timesTwo)
      val expected = EH.Left(exception)

      assert(actual == expected)
    }
  }

  describe("FlatMap") {
    def timesTwo(i: Int): Int = i * 2

    def timesTwoIfGreaterThanTwo(i: Int): EH.Either[Exception, Int] = {
      if (i > 2) {
        EH.Right(timesTwo(i))
      } else {
        EH.Left(new Exception("Input parameter was two or less"))
      }
    }

    it("Returns the original left Left if called on a left") {
      val exception = new Exception("An Error Occurred")
      val actual = EH.Left(exception).flatMap(timesTwoIfGreaterThanTwo)

      assert(actual.isInstanceOf[EH.Left[Exception]])
      actual match {
        case EH.Left(e: Exception) => assert(e.getMessage == "An Error Occurred")
        case _ => assert(1 == 2)
      }
    }

    it("Returns Left the inner function returns a left") {
      val actual = EH.Right(1).flatMap(timesTwoIfGreaterThanTwo)

      assert(actual.isInstanceOf[EH.Left[Exception]])
      actual match {
        case EH.Left(e: Exception) => assert(e.getMessage == "Input parameter was two or less")
        case _ => assert(1 == 2)
      }
    }

    it("Returns Right if called on a right, and the inner function returns right.") {
      val actual = EH.Right(3).flatMap(timesTwoIfGreaterThanTwo)
      val expected = EH.Right(6)

      assert(actual == expected)
    }
  }
}

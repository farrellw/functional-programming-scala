package chapterThree

//DS Short for DataStructures
object DS {

  //sealed trait means all implementations of List must be in this file.
  sealed trait List[+A]

  case object Nil extends DS.List[Nothing]

  case class Cons[+A](head: A, tail: DS.List[A]) extends DS.List[A]

  //Scala convention to declare a companion object in addition to our List data type.
  //Functions that deal with the List data type would be placed in this companion object.
  object List {

    //Functions placed in the object List are called Companion object to List
    def sum(ints: DS.List[Int]): Int = ints match {
      case DS.Nil => 0
      case DS.Cons(x, xs) => x + sum(xs)
    }

    //The book provides two different syntax for defining a function.
    //One with curly brackets { and one only with a new line.
    //When to use which? Seen in product and apply
    def product(ds: DS.List[Double]): Double = ds match {
      case DS.Nil => 1.0
      case DS.Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): DS.List[A] =
      if (as.isEmpty) DS.Nil
      else DS.Cons(as.head, apply(as.tail: _*))

    def tail[A](ds: DS.List[A]): DS.List[A] = ds match {
      case DS.Nil => DS.Nil
      case DS.Cons(_, t) => t
    }

    def setHead[A](newHead: A, ds: DS.List[A]): DS.List[A] = ds match {
      case DS.Nil => DS.Cons(newHead, DS.Nil)
      case DS.Cons(_, t) => DS.Cons(newHead, t)
    }

    def drop[A](n: Int, ds: DS.List[A]): DS.List[A] = {
      if(n <= 0) ds
      else drop(n - 1, tail(ds))
    }
  }
}

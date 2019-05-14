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
    def sum(ints: DS.List[Int]): Int = {
      foldRight(ints, 0)((x, y) => x + y)
    }

    //The book provides two different syntax for defining a function.
    //One with curly brackets { and one only with a new line.
    //When to use which? Seen in product and apply
    def product(ds: DS.List[Double]): Double = {
      foldRight(ds, 1.0)((x, y) => x * y)
    }

    def length[A](ds: DS.List[A]): Int = {
      foldRight(ds, 0)((_, y) => 1 + y)
    }

    //appendViaFoldRight copied from answer section
    def appendFoldRight[A](l: List[A], r: List[A]): List[A] =
      foldRight(l, r)(Cons(_, _))

    def foldRight[A, B](ds: DS.List[A], fallbackValue: B)(f: (A, B) => B): B = ds match {
      case DS.Nil => fallbackValue
      case DS.Cons(x, xs) => f(x, foldRight(xs, fallbackValue)(f))
    }


    def sumFoldLeft(ints: DS.List[Int]): Int = {
      foldLeft(ints, 0)((x, y) => x + y)
    }

    def productFoldLeft(ds: DS.List[Double]): Double = {
      foldLeft(ds, 1.0)((x, y) => x * y)
    }

    def lengthFoldLeft[A](ds: DS.List[A]): Int = {
      foldLeft(ds, 0)((x, _) => x + 1)
    }

    //reverse copied via answer section
    def reverse[A](ds: DS.List[A]): DS.List[A] = {
      foldLeft(ds, List[A]())((acc, h) => Cons(h, acc))
    }

    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case DS.Nil => z
        case DS.Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
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
      if (n <= 0) ds
      else drop(n - 1, tail(ds))
    }

    def dropWhile[A](ds: DS.List[A])(f: A => Boolean): DS.List[A] = ds match {
      case DS.Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => ds
    }

    def append[A](a1: DS.List[A], a2: DS.List[A]): DS.List[A] = a1 match {
      case DS.Nil => a2
      case DS.Cons(h, t) => DS.Cons(h, append(t, a2))
    }

    // Have to look two ahead in each loop.
    // When hit nil for the next of next, return head without tail
    def init[A](ds: DS.List[A]): DS.List[A] = ds match {
      case DS.Cons(h, t) => {
        t match {
          case DS.Cons(_, tt) =>
            tt match {
              case DS.Nil => DS.Cons(h, DS.Nil)
              case _ => DS.Cons(h, init(t))
            }
          case DS.Nil => DS.Nil
        }
      }
      case _ => DS.Nil
    }

    def map[A, B](as: List[A])(f: A => B): List[B] = as match {
      case DS.Nil => DS.Nil
      case DS.Cons(h, t) => DS.Cons(f(h), map(t)(f))
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case DS.Cons(h, t) if f(h) => DS.Cons(h, filter(t)(f))
      case DS.Cons(h, t) => filter(t)(f)
      case DS.Nil => DS.Nil
    }

    def flatMap[A](as: List[A])(f: A => List[A]): List[A] = as match {
      case DS.Nil => DS.Nil
      case DS.Cons(h, t) => {
        append(f(h), flatMap(t)(f))
      }
    }

    def zipWith[A](as: List[A], as2: List[A])(f: (A, A) => A): List[A] = {
      as match {
        case DS.Nil => DS.Nil
        case DS.Cons(h, t) => {
          as2 match {
            case DS.Nil => DS.Nil
            case DS.Cons(hh, tt) => {
              DS.Cons(f(h, hh), zipWith(t, tt)(f))
            }
          }
        }
      }
    }

    def take[A](as: DS.List[A], n: Int): DS.List[A] = {
      as match {
        case DS.Cons(h, t) if n > 0 => DS.Cons(h, take(t, n - 1))
        case _ => DS.Nil
      }
    }

    def takeWhile[A](as: DS.List[A])(f: A => Boolean): DS.List[A] = {
      as match {
        case DS.Cons(h, t) if f(h) => DS.Cons(h, takeWhile(t)(f))
        case _ => DS.Nil
      }
    }

    def forall[A](as: DS.List[A])(f: A => Boolean): Boolean = {
      as match {
        case DS.Nil => true
        case DS.Cons(h, t) if f(h) => forall(t)(f)
        case _ => false
      }
    }
  }

}

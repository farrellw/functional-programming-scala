package chapterFive

object LZ {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case LZ.Empty => None
      case LZ.Cons(h, t) => Some(h())
    }

    def toList: List[A] = this match {
      case LZ.Empty => Nil
      case LZ.Cons(h, t) => {
        List(h()) ++ t().toList
      }
    }

    def take(n: Int): LZ.Stream[A] = this match {
      case LZ.Cons(h, t) if n - 1 >= 0 => LZ.Cons(h, () => t().take(n - 1))
      case _ => LZ.Empty
    }

    def drop(n: Int): LZ.Stream[A] = this match {
      case LZ.Cons(_, t) if n >= 1 => t().drop(n - 1)
      case _ => this
    }

    def takeWhile(f: A => Boolean): LZ.Stream[A] = this match {
      case LZ.Cons(h, t) if f(h()) => LZ.Cons(h, () => t().takeWhile(f))
      case _ => LZ.Empty
    }

    def takeWhileUsingFoldRight(f: A => Boolean): LZ.Stream[A] =  {
      foldRight(Stream.empty[A])((h,t) => {
        if (f(h)) {
          LZ.Cons(() => h,() => t)
        }
        else {
          LZ.Empty
        }
      })
    }

    def headOptionFoldRight: Option[A] =
      foldRight(None: Option[A])((h,_) => Some(h))

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case LZ.Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean = this match {
      case LZ.Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def forAll(p: A => Boolean): Boolean = this match {
      case LZ.Cons(h, t) => {
        if(p(h())){
          t().forAll(p)
        } else {
          false
        }
      }
      case _ => true
    }

    def existsUsingFoldRight(p: A => Boolean): Boolean = {
      foldRight(false)((a, b) => p(a) || b)
    }
  }

  case object Empty extends LZ.Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => LZ.Stream[A]) extends LZ.Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => LZ.Stream[A]): LZ.Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      LZ.Cons(() => head, () => tail)
    }

    def empty[A]: LZ.Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }


}
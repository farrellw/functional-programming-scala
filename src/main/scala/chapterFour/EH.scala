package chapterFour

object EH {

  sealed trait Option[+A] {
    def map[B](f: A => B): EH.Option[B] = this match {
      case EH.Some(a) => EH.Some(f(a))
      case EH.None => EH.None
    }

    def flatMap[B](f: A => EH.Option[B]): EH.Option[B] = this match {
      case EH.Some(a) => f(a)
      case EH.None => EH.None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case EH.Some(a) => a
      case EH.None => default
    }

    def orElse[B >: A](ob: => EH.Option[B]): EH.Option[B] = this match {
      case EH.Some(a) => EH.Some(a)
      case EH.None => ob
    }

    def filter(f: A => Boolean): EH.Option[A] = this match {
      case EH.Some(a) if f(a) => EH.Some(a)
      case _ => EH.None
    }

  }

  case class Some[+A](get: A) extends EH.Option[A]

  case object None extends EH.Option[Nothing]

  def mean(xs: Seq[Double]): EH.Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): EH.Option[Double] = {
    val me = mean(xs)
    me.flatMap(m => mean(xs.map((a: Double) => {
      math.pow(a - m, 2)
    })))
  }

  def map2[A, B, C](a: EH.Option[A], b: EH.Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def sequence[A](a: List[EH.Option[A]]): EH.Option[List[A]] = {
    val newSequence = a.map(_ match {
      case EH.None => return EH.None
      case EH.Some(i) => i
    })
    EH.Some(newSequence)
  }

  def traverse[A, B](a: List[A])(f: A => EH.Option[B]): EH.Option[List[B]] = {
    val newList = a.map(aa => f(aa) match {
      case EH.None => return EH.None
      case EH.Some(b) => b
    })
    EH.Some(newList)
  }

  def sequenceInTermsOfTraverse[A](a: List[EH.Option[A]]): EH.Option[List[A]] = {
    traverse(a)(b => b.orElse(EH.None))
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): EH.Either[E, B] = this match {
      case EH.Left(e: E) => EH.Left(e)
      case EH.Right(a: A) => EH.Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => EH.Either[EE, B]): EH.Either[EE, B] = this match {
      case EH.Left(e: E) => EH.Left(e)
      case Right(a: A) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => EH.Either[EE, B]): EH.Either[EE, B] = ???

    def map2[EE >: E, B, C](b: EH.Either[EE, B])(f: (A, B) => C): EH.Either[EE, C] = ???
  }

  case class Left[+E](value: E) extends EH.Either[E, Nothing]

  case class Right[+A](value: A) extends EH.Either[Nothing, A]

}
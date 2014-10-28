package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(x, xs) => xs
      case _ => Nil
    }
  }

  def setHead[A](l: List[A], newHead: A): List[A] = {
    l match {
      case Cons(x, xs) => Cons(newHead, xs)
      case _ => Nil
    }
  }

  def drop[A](l: List[A], num: Int): List[A] = {
    if (num <= 0) l
    else l match {
      case Cons(x, xs) => drop(xs, num - 1)
      case _ => Nil
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =  {
    l match {
      case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else l
      case _ => Nil
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of an empty list!")
      case Cons(x,  Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  def reverse[A](l: List[A]) : List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit  = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*)
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def add1(l: List[Int]): List[Int] = map(l)(x => x + 1)

  def doubleToString(l: List[Double]) : List[String] = {
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))
  }

  def mapAgain[A, B](as: List[A])(f: A => B) : List[B] = {
    foldRight(as, Nil:List[B])((h, t) => Cons(f(h), t))
  }

  def filter[A](as: List[A])(f: A => Boolean) : List[A] = {
    foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def concat[A](l: List[List[A]]) : List[A] = {
    foldRight(l, Nil:List[A])(append)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]) : List[B] = {
    concat(map(as)(f))
  }

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def pairSum(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, pairSum(t1, t2))
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => false
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], subseq: List[A]): Boolean = l match {
    case Nil => false
    case Cons(h, t) if startsWith(l, subseq) => true
    case Cons(h, t) => hasSubsequence(t, subseq)
  }
}

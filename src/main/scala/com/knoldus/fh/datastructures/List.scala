package com.knoldus.fh.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

// Companion Object
object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => foldRight(xs, f(x,z))(f)
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case (Cons(_, t)) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil                => Cons(h, Nil)
    case (Cons(head, tail)) => Cons(h, Cons(head, tail))
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil                => Nil
    case (Cons(head, tail)) => if (n == 0) l else drop(tail, n - 1)
  }
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x,y)=>y+1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =  l match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  def map[A, B](l: List[A])(f: A => B): List[B] = ???

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((acc, h) => Cons(h, acc))
  
  
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  def doubleToString[Double](l: List[Double]): List[String] = foldRight(l,List[String]())((h,t)=>Cons(h.toString,t))
  
  def map2[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())((h,t)=>Cons(f(h),t))
  
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((h,t)=>{if(f(h)) Cons(h,t) else t})
  
}




object ListApp extends App {
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + List.sum(t)
    case _                                     => 101
  }

  println(List.setHead(List(1, 2, 3, 4), 5))
  println(List.tail(List(1, 2, 3, 4)))
  println(List.drop(List(1, 2, 3, 4), 2))
  println(List.init(List(1, 2, 3, 4)))
  println(List.dropWhile(List(2, 2, 3, 4),(x:Int)=>x %2 ==0))
  println(List.length(List(2, 2, 3, 4)))
  println(List.reverse(List(1,2,3,4)))
  println(List.append2(List(1,2,3,4), List(5,6,7,8)))
  println(List.doubleToString(List(1.0,2.0,3.0,4.0)))
  println(List.filter(List(2, 2, 3, 4))((x:Int)=>x %2 ==0))
  
}

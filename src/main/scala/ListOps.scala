package net.gebski.FuncScalaSandbox.Chapter3

import scala.annotation.tailrec

object ListOps {
  def tail[A](l: List[A]) : List[A] = {
    l match {
      case List() => List()
      case List(a) => List()
      case a :: b => b
    }
  }

  def setHead[A](l: List[A], a: A) : List[A] = {
    l match {
      case List() => l
      case List(x) => List(a)
      case x :: y => y.+:(a)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
    l match {
      case List() => l
      case x :: y => drop(y, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case List() => l
      case x :: y => if (f(x)) dropWhile(y, f) else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case List() => l
      case List(x) => List()
      case x :: y => init(y).+:(x)
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  def lengthFoldRight[A](as: List[A]): Int = {
    foldRight(as, 0) {(a: A, b: Int) => { b + 1 }}
  }

  def sumFoldRight(ints: List[Int]): Int = {
    foldRight(ints, 0) {(a: Int, b: Int) => { a + b } }
  }

  def productFoldRight(ds: List[Int]): Int = {
    ds match {
      case Nil => 0
      case ds_ => foldRight(ds_, 1) {(a: Int, b: Int) => { a * b } }
    }
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def lengthFoldLeft[A](as: List[A]): Int = {
    foldLeft(as, 0) {(a: Int, b: A) => { a + 1 } }
  }

  def sumFoldLeft(ints: List[Int]): Int = {
    foldLeft(ints, 0) {(a: Int, b: Int) => { a + b } }
  }

  def productFoldLeft(ds: List[Int]): Int = {
    ds match {
      case Nil => 0
      case ds_ => foldLeft(ds_, 1) {(a: Int, b: Int) => { a * b } }
    }
  }

  def reverse[A](ls: List[A]): List[A] = {
    ls match {
      case Nil => Nil
      case x :: Nil => x :: Nil
      case x :: xs => reverse(xs) :+ x
    }
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = ???

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = ???

  def lengthFoldRight2[A](as: List[A]): Int = {
    foldRight2(as, 0) {(a: A, b: Int) => { b + 1 }}
  }

  def sumFoldRight2(ints: List[Int]): Int = {
    foldRight2(ints, 0) {(a: Int, b: Int) => { a + b } }
  }

  def productFoldRight2(ds: List[Int]): Int = {
    ds match {
      case Nil => 0
      case ds_ => foldRight2(ds_, 1) {(a: Int, b: Int) => { a * b } }
    }
  }

  def lengthFoldLeft2[A](as: List[A]): Int = {
    foldLeft2(as, 0) {(a: Int, b: A) => { a + 1 } }
  }

  def sumFoldLeft2(ints: List[Int]): Int = {
    foldLeft2(ints, 0) {(a: Int, b: Int) => { a + b } }
  }

  def productFoldLeft2(ds: List[Int]): Int = {
    ds match {
      case Nil => 0
      case ds_ => foldLeft2(ds_, 1) {(a: Int, b: Int) => { a * b } }
    }
  }

  def append[A](ls: List[A], l: A): List[A] = {
    foldRight(ls, l :: Nil)((x: A, agg: List[A]) => agg.+:(x))
  }

  def concatenate[A](ls: List[List[A]]): List[A] = {
    foldLeft(ls, List[A]()){
      (l: List[A], e: List[A]) => {
        e.:::(l)
      }
    }
  }
}

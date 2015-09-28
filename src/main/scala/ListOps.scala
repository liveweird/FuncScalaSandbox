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

  def increment(ls: List[Int]): List[Int] = {
    foldRight(ls, List[Int]()){
      (e: Int, l: List[Int]) => {
        l.::(e + 1)
      }
    }
  }

  def stringize(ls: List[Double]): List[String] = {
    ls match {
      case Nil => Nil
      case x :: xs => x.toString() :: stringize(xs)
    }
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => f(x) :: ListOps.map(xs)(f)
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case x :: xs if f(x) => x :: ListOps.filter(xs)(f)
      case x :: xs => ListOps.filter(xs)(f)
    }
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => f(x) ::: ListOps.flatMap(xs)(f)
    }
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    ListOps.flatMap[A,A](as)(x => if (f(x)) x :: Nil else List[A]())
  }
}

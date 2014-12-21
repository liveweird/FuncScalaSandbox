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
}

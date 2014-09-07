package net.gebski.FuncScalaSandbox.Chapter3

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
      case x :: nil  => List(a)
      case x :: y => a :: y
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
    l
  }

  def init[A](l: List[A]): List[A] = {
    l
  }
}

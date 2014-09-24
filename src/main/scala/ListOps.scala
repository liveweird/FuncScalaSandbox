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
}

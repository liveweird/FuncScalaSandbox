package net.gebski.FuncScalaSandbox.Chapter3

object ListOps {
  def tail[A](l: List[A]) : List[A] = {
    l match {
      case List() => List()
      case List(a) => List()
      case a :: b => b
    }
  }
}

package net.gebski.FuncScalaSandbox.Chapter2

object IsSorted {
  def isSorted[A <: Int](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    as match {
      case Array() => true
      case Array(_) => true
      case _ => (as zip as.drop(1)).foldLeft(true)((a, b) => { a &&  b._1 <= b._2 })
    }
  }
}

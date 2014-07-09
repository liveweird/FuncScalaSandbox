import scala.annotation.tailrec

object Fibonacci {
  def fib(n: Int): Int = {
    n match {
      case 1 => 1
      case 2 => 1
      case _ => smartFib(1, 1, 3, n)
    }
  }

  @tailrec
  private def smartFib(fa: Int, fb: Int, k: Int, n: Int): Int = {
    if (k == n) {
      fa + fb
    } else {
      smartFib(fb, fa + fb, k + 1, n)
    }
  }
}

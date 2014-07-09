import org.scalatest.FunSpec

class FibonacciSpec extends FunSpec {

  describe("Fibonacci calculation") {

    it ("should be 1 for 1") {
      assert(Fibonacci.fib(1) == 1)
    }

    it("should be 1 for 2") {
      assert(Fibonacci.fib(2) == 1)
    }

    it("should be fib(n-1) + fib(n-2) for n") {
      for (i <- 3 to 10) {
        val a = Fibonacci.fib(i)
        val b = Fibonacci.fib(i - 1)
        val c = Fibonacci.fib(i - 2)
        assert(a == b + c, "; n = " + i + "; f(n) = " + a + "; f(n-1) = " + b + "; f(n-2) = " + c)
      }
    }
  }
}

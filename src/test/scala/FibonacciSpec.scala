import org.scalatest.FunSpec

class FibonacciSpec extends FunSpec {

  describe("Fibonacci calculation") {

    it ("should be 1 for 1") {
      assert(Fibonacci.fib(1) == 1)
    }

    it("should be 1 for 2") {
      assert(Fibonacci.fib(2) == 1)
    }
  }
}

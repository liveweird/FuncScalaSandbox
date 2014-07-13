package net.gebski.FuncScalaSandbox.Chapter2

import org.scalatest.FunSpec

class CompositionSpec extends FunSpec {
  describe("Currying") {
    it("works for A + B = C") {
      val func1 = (a: Int, b: Int) => { a + b }
      assert(func1(2,3) == Composition.curry(func1)(2)(3))
    }
  }

  describe("Uncurrying") {
    it("works for A + B = C") {
      val func1 = (a: Int) => (b: Int) => { a + b }
      assert(func1(2)(3) == Composition.uncurry(func1)(2,3))
    }
  }
}

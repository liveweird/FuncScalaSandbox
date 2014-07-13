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

  describe("Compose") {
    it("works for y = x + 2, z = y * 3") {
      val func1 = (a: Int) => a + 2
      val func2 = (a: Int) => a * 3
      assert(func1(func2(7)) == Composition.compose(func1, func2)(7))
    }
  }
}

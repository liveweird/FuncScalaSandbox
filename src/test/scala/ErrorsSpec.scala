package net.gebski.FuncScalaSandbox.Chapter4

import net.gebski.FuncScalaSandbox.Chapter4.Option
import org.scalatest.FunSpec

import scala.{Option => _, Some => _, Either => _, _}

class ErrorsSpec extends FunSpec {

  // 4.1
  describe("Options - mapping") {
    it("Works for some") {
      val option = Some(3)
      assert(Some(4) == option.map((a: Int) => a + 1))
    }

    it("Works for none") {
      val option = None
      assert(None == option.map((a: Int) => a + 1))
    }
  }

  describe("Options - getOrElse") {
    it("Works for some") {
      val option = Some(4)
      assert(4 == option.getOrElse(3))
    }

    it("Works for none") {
      val option = None
      assert(3 == option.getOrElse(3))
    }
  }

  describe("Options - flatmapping") {
    it("Works for some") {
      val option = Some(1)
      assert(Some(1) == option.flatMap((a: Int) => Some(a)))
    }

    it("Works for none") {
      val option = None
      assert(None == option.flatMap((a: Int) => Some(a)))
    }
  }
}
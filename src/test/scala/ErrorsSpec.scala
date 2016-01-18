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
}
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

  describe("Options - orElsing") {
    it("Works for some") {
      val option = Some(1)
      assert (Some(1) == option.orElse(Some(2)))
    }

    it("Works for none") {
      val option = None
      assert (Some(2) == option.orElse(Some(2)))
    }

    it("Works for none & none") {
      val option = None
      assert (None == option.orElse(None))
    }
  }

  describe("Options - filtering") {
    it("Works for some, met") {
      val option = Some(1)
      assert(Some(1) == option.filter((a: Int) => a > 0))
    }

    it("Works for some, not met") {
      val option = Some(1)
      assert(None == option.filter((a: Int) => a < 0))
    }

    it("Works for none") {
      val option = None
      assert(None == option.flatMap((a: Int) => Some(a)))
    }
  }

  describe ("Option - variance") {
    it("For empty sequence") {
      assert(None == Option.variance(List[Double] ()))
    }

    it("For a single element in the sequence") {
      assert(Some(0) == Option.variance(List[Double] (1)))
    }

    it("For two elements in the sequence") {
      assert(Some(1) == Option.variance(List[Double] (1,3)))
    }

    it("For three elements in the sequence") {
      assert(Some(2.0/3) == Option.variance(List[Double] (1,2,3)))
    }
  }
}
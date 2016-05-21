package net.gebski.FuncScalaSandbox.Chapter4

import org.scalatest.FunSpec

import scala.{Option => _, Some => _, Either => _, _}

class ErrorsSpec extends FunSpec {

  def assertListCmp[A](l1: List[A], l2: List[A]) = {
    assert(l1.length == l2.length)
    assert(0 == l1.zip(l2).count { case (x, y) => x != y} )
  }

  def assertListOptionCmp[A](l1: Option[List[A]], l2: Option[List[A]]) = {
    l1 match {
      case _l1: Some[List[A]] =>
        l2 match {
          case _l2: Some[List[A]] =>
            assertListCmp(_l1.get, _l2.get)
          case _ => assert(false)
        }
      case _ =>
        l2 match {
          case None => ;
          case _ => assert(false)
        }
    }
  }

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

  describe ("Options - variance") {
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

  describe("Options - map2") {
    it("First value empty") {
      assert(None == Option.map2(None, Some(1))((a: Int, b: Int) => a + b))
    }

    it("Second value empty") {
      assert(None == Option.map2(Some(2), None)((a: Int, b: Int) => a + b))
    }

    it("Both values empty") {
      assert(None == Option.map2(None, None)((a: Int, b: Int) => a + b))
    }

    it("None of the values empty") {
      assert(Some(3) == Option.map2(Some(1), Some(2))((a: Int, b: Int) => a + b))
    }
  }

  describe("Options - sequence") {
    it("Empty list") {
      val empty = List[Int]()
      val emptyOpts = List[Option[Int]]()
      assertListOptionCmp(Some(empty), Option.sequence(emptyOpts))
    }

    it("Single None") {
      assertListOptionCmp(None, Option.sequence(List[Option[Int]](None)))
    }

    it("Single Some") {
      assertListOptionCmp(Some(List[Int](3)), Option.sequence(List[Option[Int]](Some(3))))
    }

    it("Only Somes") {
      assertListOptionCmp(Some(List[Int](3, 5, 7)), Option.sequence(List[Option[Int]](Some(3), Some(5), Some(7))))
    }

    it("Somes with a single None") {
      assertListOptionCmp(None, Option.sequence(List[Option[Int]](Some(3), None, Some(7))))
    }

    it("Somes with some Nones") {
      assertListOptionCmp(None, Option.sequence(List[Option[Int]](None, Some(3), Some(5), None, Some(7))))
    }

    it("Some Nones") {
      assertListOptionCmp(None, Option.sequence(List[Option[Int]](None, None, None)))
    }
  }

  describe("Options - traverse") {
    it("Empty list") {
      val func = (x: Int) => { if (x > 0) Some(x) else None }
      val empty = List[Int]()
      val emptyOpts = Some(List[Int]())
      assertListOptionCmp(emptyOpts, Option.traverse(empty)(func))
    }

    it("Non-empty list - maps all to None") {
      val func = (x: Int) => { if (x > 0) Some(x) else None }
      val empty = List[Int](-3, -5, -6)
      val emptyOpts = Some(List[Int]())
      assertListOptionCmp(emptyOpts, Option.traverse(empty)(func))
    }

    it("Non-empty list - maps all to Some") {
      val func = (x: Int) => { if (x > 0) Some(x) else None }
      val empty = List[Int](3, 5, 6, 7, 9)
      val emptyOpts = Some(List[Int](3, 5, 6, 7, 9))
      assertListOptionCmp(emptyOpts, Option.traverse(empty)(func))
    }

    it("Non-empty list - maps some to Some, some to None") {
      val func = (x: Int) => { if (x > 0) Some(x) else None }
      val empty = List[Int](-3, 5, -6, 7, 9, 0)
      val emptyOpts = Some(List[Int](5, 7, 9))
      assertListOptionCmp(emptyOpts, Option.traverse(empty)(func))
    }
  }

  // 4.6
  describe("Either - map") {
    it("Error maps to error") {
      val input = Left("dsda")
      assert(input == input.map[Int]((x: Int) => x + 1))
    }

    it("Value maps to value") {
      val input = Right(4)
      assert(Right(5) == input.map[Int]((x: Int) => x + 1))
    }
  }
}
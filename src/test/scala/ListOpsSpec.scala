package net.gebski.FuncScalaSandbox.Chapter3

import org.scalatest.FunSpec

class ListOpsSpec extends FunSpec {
  describe("Returning all but the first element") {
    it("on empty list returns nothing") {
      assert(0 == ListOps.tail(List[Int]()).length)
    }

    it("on 1 element long list returns nothing") {
      assert(0 == ListOps.tail(List[Int](1)).length)
    }

    it("on list longer than 1 element, returns the actual tail") {
      val list1 = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
      val list2 = 2 :: 3 :: 4 :: 5 :: Nil
      assert(0 == list2.diff(ListOps.tail(list1)).length)
    }
  }

  describe("Setting the 1st element") {
    it("on empty list returns nothing") {
      assert(0 == ListOps.setHead(List[Int](), 1).length)
    }

    it("on 1 element long list returns just the new value") {
      val list = 1 :: Nil
      val expected = 2 :: Nil
      val replaced = ListOps.setHead(list, 2)
      assert(1 == replaced.length)
      assert(0 == replaced.zip(expected).count{case (x,y) => x != y})
    }

    it("on longer list returns just the new value") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 9 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val replaced = ListOps.setHead(list, 9)
      assert(1 == replaced.length)
      assert(0 == replaced.zip(expected).count{case (x,y) => x != y})
    }
  }
}

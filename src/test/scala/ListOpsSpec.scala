package net.gebski.FuncScalaSandbox.Chapter3

import org.scalatest.FunSpec

class ListOpsSpec extends FunSpec {
  describe("Executing the tail method") {
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
}

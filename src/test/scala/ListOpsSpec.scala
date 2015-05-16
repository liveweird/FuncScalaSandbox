package net.gebski.FuncScalaSandbox.Chapter3

import org.scalatest.FunSpec

class ListOpsSpec extends FunSpec {

  def assertListCmp[A](l1: List[A], l2: List[A]) = {
    assert(l1.length == l2.length)
    assert(0 == l1.zip(l2).count { case (x, y) => x != y} )
  }

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
      val result = ListOps.setHead(list, 2)
      assertListCmp(expected, result)
    }

    it("on longer list returns just the new value") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 9 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val result = ListOps.setHead(list, 9)
      assertListCmp(expected, result)
    }
  }

  describe("Dropping first n elements") {
    it("on empty lists return nothing") {
      assert(0 == ListOps.drop(List(), 3).length)
    }

    it("on list shorter than n returns nothing") {
      assert(0 == ListOps.drop(1 :: 2 :: 3 :: Nil, 4).length)
    }

    it("on list as long as n returns nothing") {
      assert(0 == ListOps.drop(1 :: 2 :: 3 :: Nil, 3).length)
    }

    it("if n <= 0 list is returned unchanged") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val result = ListOps.drop(list, -3)
      assertListCmp(expected, result)
    }

    it("if n < than list's lenght, shortened list is returned") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 4 :: 5 :: 6 :: Nil
      val result = ListOps.drop(list, 3)
      assertListCmp(expected, result)
    }
  }

  describe("Dropping elements until the condition is met") {
    it ("on empty list returns nothing") {
      assert(0 == ListOps.dropWhile(List(), (p: Int) => true).length)
    }

    it("if condition is never met, returns original list") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val result = ListOps.dropWhile(list, (p: Int) => false)
      assertListCmp(expected, result)
    }

    it("if condition is met on the element 1 (only, returns original list starting with element 2") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val result = ListOps.dropWhile(list, (p: Int) => p <= 1)
      assertListCmp(expected, result)
    }

    it("if condition is met on elements 1 & 3, returns original list starting with element 2") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val result = ListOps.dropWhile(list, (p: Int) => (p == 1 || p == 3))
      assertListCmp(expected, result)
    }

    it("if condition is met on first n elements, drop first n - 1 elements") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 4 :: 5 :: 6 :: Nil
      val result = ListOps.dropWhile(list, (p: Int) => p < 4)
      assertListCmp(expected, result)
    }

    it("all elements meet conditions, result is empty") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = List()
      val result = ListOps.dropWhile(list, (p: Int) => true)
      assertListCmp(expected, result)
    }
  }

  describe("Dropping only the last element of the collection") {
    it("on empty list return nothing") {
      assert(0 == ListOps.init(List()).length)
    }

    it("on list with 1 element, returns nothing") {
      assert(0 == ListOps.init(1 :: Nil).length)
    }

    it("on list with more than 1 element, returns all elements but the last one") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
      val expected = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
      val result = ListOps.init(list)
      assertListCmp(expected, result)
    }
  }

  describe("Calculating the length of list") {
    describe("using foldRight") {
      it ("on empty list it's 0") {
        assert(0 == ListOps.lengthFoldRight(List()))
      }

      it ("on list with 1 element it's just 1") {
        assert(1 == ListOps.lengthFoldRight(1 :: Nil))
      }

      it ("on list with n elements, it's n") {
        assert(4 == ListOps.lengthFoldRight(1 :: 10 :: 100 :: 1000 :: Nil))
      }
    }

    describe("using foldLeft") {
      it ("on empty list it's 0") {
        assert(0 == ListOps.lengthFoldLeft(List()))
      }

      it ("on list with 1 element it's just 1") {
        assert(1 == ListOps.lengthFoldLeft(1 :: Nil))
      }

      it ("on list with n elements, it's n") {
        assert(4 == ListOps.lengthFoldLeft(1 :: 10 :: 100 :: 1000 :: Nil))
      }
    }
  }

  describe("Calculating sum of elements in list") {
    describe("using foldLeft") {
      it ("Empty list returns 0") {
        assert(0 == ListOps.sumFoldLeft(List()))
      }

      it ("One value is just this value") {
        assert(4 == ListOps.sumFoldLeft(4 :: Nil))
      }

      it ("More than one value returns the actual sum") {
        assert(10 == ListOps.sumFoldLeft(1 :: 3 :: 6 :: Nil))
      }
    }

    describe("using foldRight") {
      it ("Empty list returns 0") {
        assert(0 == ListOps.sumFoldRight(List()))
      }

      it ("One value is just this value") {
        assert(4 == ListOps.sumFoldRight(4 :: Nil))
      }

      it ("More than one value returns the actual sum") {
        assert(10 == ListOps.sumFoldRight(1 :: 3 :: 6 :: Nil))
      }
    }
  }

  describe("Calculating product of elements in list") {
    describe("using foldLeft") {
      it ("Empty lists return 0") {
        assert(0 == ListOps.productFoldLeft(List()))
      }

      it ("One value is just this value") {
        assert(4 == ListOps.productFoldLeft(4 :: Nil))
      }

      it ("More than one value returns the actual product") {
        assert(30 == ListOps.productFoldLeft(1 :: 5 :: 6 :: Nil))
      }

      it ("Zero nullifies the result") {
        assert(0 == ListOps.productFoldLeft(1 :: 5 :: 0 :: 6 :: Nil))
      }
    }

    describe("using foldRight") {
      it ("Empty lists return 0") {
        assert(0 == ListOps.productFoldRight(List()))
      }

      it ("One value is just this value") {
        assert(4 == ListOps.productFoldRight(4 :: Nil))
      }

      it ("More than one value returns the actual product") {
        assert(30 == ListOps.productFoldRight(1 :: 5 :: 6 :: Nil))
      }

      it ("Zero nullifies the result") {
        assert(0 == ListOps.productFoldRight(1 :: 5 :: 0 :: 6 :: Nil))
      }
    }
  }

  describe("Reverse") {
    it ("Empty list reversed is an empty list") {
      assert(Nil == ListOps.reverse(Nil))
    }

    it ("1 elem-long list reversed is itself") {
      val list = 1 :: Nil
      val result = ListOps.reverse(list)
      assertListCmp(list, result)
    }

    it ("longer list reversed is proper") {
      val list = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
      val expected = 5 :: 4 :: 3 :: 2 :: 1 :: Nil
      val result = ListOps.reverse(list)
      assertListCmp(expected, result)
    }
  }

  describe("Append") {
    it("Append element to an empty list") {
      val expected = 1 :: Nil
      assertListCmp(expected, ListOps.append(List(), 1))
    }

    it("Append to a non-empty list") {
      val list = 1 :: 2 :: 3 :: 4 :: Nil
      val expected = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
      assertListCmp(expected, ListOps.append(list, 5))
    }
  }

  describe("Concatenate") {
    it("Empty lists") {
      assertListCmp(Nil, ListOps.concatenate(List(List(),List(),List())))
    }

    it("Just 1 list") {
      val list = 1 :: 2 :: 3 :: Nil
      assertListCmp(list, ListOps.concatenate(List(list)))
    }

    it("Just 1 list & empties") {
      val list = 1 :: 2 :: 3 :: Nil
      assertListCmp(list, ListOps.concatenate(List(list, Nil, Nil)))
    }

    it("Just empties & 1 list") {
      val list = 1 :: 2 :: 3 :: Nil
      assertListCmp(list, ListOps.concatenate(List(Nil, Nil, list)))
    }

    it("Just 3 lists") {
      val list1 = 1 :: 2 :: 3 :: Nil
      val list2 = 4 :: 5 :: 6 :: 7 :: Nil
      val list3 = 8 :: 9 :: Nil
      val expected = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil
      assertListCmp(expected, ListOps.concatenate(List(list1, list2, list3)))
    }
  }

  describe("Increment") {
    it("Does nothing for the empty list") {
      val expected = Nil
      assertListCmp(expected, ListOps.increment(expected))
    }

    it("Increments each item in a non-empty list") {
      val list = 1 :: 2 :: 3 :: 4 :: Nil
      val expected = 2 :: 3 :: 4 :: 5 :: Nil
      assertListCmp(expected, ListOps.increment(list))
    }
  }
}
package net.gebski.FuncScalaSandbox.Chapter3

import net.gebski.FuncScalaSandbox.Chapter3.ListOps.{Tree, Branch, Leaf}
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

  describe("Stringize") {
    it("Does nothing for the empty list") {
      val expected = Nil
      assertListCmp(expected, ListOps.stringize(expected))
    }

    it("Stringizes doubles in order") {
      val list = 1.043 :: 22.0E44 :: -33.12 :: Nil
      val expected = (1.043).toString() :: (22.0E44).toString() :: (-33.12).toString() :: Nil
      assertListCmp(expected, ListOps.stringize(list))
    }
  }

  describe("Map") {
    it("Does nothing for the empty list") {
      val expected = List[Int]()
      def functor = (x: Int) => x + 1
      assertListCmp(expected, ListOps.map(expected)(functor))
    }

    it("Properly maps all the items in the list") {
      val list = 1 :: 3 :: 5 :: -2 :: Nil
      val expected = 2 :: 4 :: 6 :: -1 :: Nil
      def functor = (x: Int) => x + 1
      assertListCmp(expected, ListOps.map(list)(functor))
    }
  }

  // 3.19
  describe("Filter") {
    it("Does nothing for the empty list") {
      val expected = List[Int]()
      def functor = (x: Int) => x > 1
      assertListCmp(expected, ListOps.filter(expected)(functor))
    }

    it("Does remove stuff that doesn't meet condition") {
      val list = 1 :: 3 :: 5 :: -2 :: Nil
      val expected = 3 :: 5 :: Nil
      def functor = (x: Int) => x > 1
      assertListCmp(expected, ListOps.filter(list)(functor))
    }

    it("Does remove stuff that doesn't meet condition & appears few times") {
      val list = 1 :: 3 :: 1 :: 5 :: -2 :: Nil
      val expected = 3 :: 5 :: Nil
      def functor = (x: Int) => x > 1
      assertListCmp(expected, ListOps.filter(list)(functor))
    }
  }

  // 3.20
  describe("FlatMap") {
    it("Does nothing when applied on empty list") {
      val expected = List[Int]()
      def functor = (x: Int) => List(x, x)
      assertListCmp(expected, ListOps.flatMap(expected)(functor))
    }

    it("Merges list when proper mapping is provided") {
      val list = 1 :: 3 :: 5 :: -2 :: Nil
      val expected = 1 :: 1 :: 3 :: 3 :: 5 :: 5 :: -2 :: -2 :: Nil
      def functor = (x: Int) => List(x, x)
      assertListCmp(expected, ListOps.flatMap(list)(functor))
    }

    it("Retrieves an empty list when mapping empties lists") {
      val list = 1 :: 3 :: 5 :: -2 :: Nil
      val expected = List[Int]()
      def functor = (x: Int) => List[Int]()
      assertListCmp(expected, ListOps.flatMap(list)(functor))
    }
  }

  // 3.21
  describe("Filter2") {
    it("Does nothing for the empty list") {
      val expected = List[Int]()
      def functor = (x: Int) => x > 1
      assertListCmp(expected, ListOps.filter2(expected)(functor))
    }

    it("Does remove stuff that doesn't meet condition") {
      val list = 1 :: 3 :: 5 :: -2 :: Nil
      val expected = 3 :: 5 :: Nil
      def functor = (x: Int) => x > 1
      assertListCmp(expected, ListOps.filter2(list)(functor))
    }

    it("Does remove stuff that doesn't meet condition & appears few times") {
      val list = 1 :: 3 :: 1 :: 5 :: -2 :: Nil
      val expected = 3 :: 5 :: Nil
      def functor = (x: Int) => x > 1
      assertListCmp(expected, ListOps.filter2(list)(functor))
    }
  }

  // 3.22
  describe("ZipWithInt") {
    it("First list empty") {
      val l1 = List[Int]()
      val l2 = 1 :: 2 :: Nil
      assertListCmp(l2, ListOps.zipWithInt(l1, l2))
    }

    it("Second list empty") {
      val l1 = List[Int]()
      val l2 = 1 :: 2 :: Nil
      assertListCmp(l2, ListOps.zipWithInt(l2, l1))
    }

    it("Left list shorter") {
      val l1 = 8 :: 7 ::Nil
      val l2 = 1 :: 2 :: 4 :: 6 :: Nil
      val expected = 9 :: 9 :: 4 :: 6 :: Nil
      assertListCmp(expected, ListOps.zipWithInt(l1, l2))
    }

    it("Right list shorter") {
      val l1 = 8 :: 7 ::Nil
      val l2 = 1 :: 2 :: 4 :: 6 :: Nil
      val expected = 9 :: 9 :: 4 :: 6 :: Nil
      assertListCmp(expected, ListOps.zipWithInt(l2, l1))
    }

    it("Equal lists") {
      val l1 = 8 :: 7 :: 0 :: 2 :: Nil
      val l2 = 1 :: 2 :: 4 :: 6 :: Nil
      val expected = 9 :: 9 :: 4 :: 8 :: Nil
      assertListCmp(expected, ListOps.zipWithInt(l1, l2))
    }
  }

  // 3.23
  describe("ZipWith") {
    it("First list empty") {
      val l1 = List[Int]()
      val l2 = 1 :: 2 :: Nil
      assertListCmp(l2, ListOps.zipWith(l1, l2)((x, y) => x + y))
    }

    it("Second list empty") {
      val l1 = List[Int]()
      val l2 = 1 :: 2 :: Nil
      assertListCmp(l2, ListOps.zipWith(l2, l1)((x, y) => x + y))
    }

    it("Left list shorter") {
      val l1 = 8 :: 7 ::Nil
      val l2 = 1 :: 2 :: 4 :: 6 :: Nil
      val expected = 9 :: 9 :: 4 :: 6 :: Nil
      assertListCmp(expected, ListOps.zipWith(l1, l2)((x, y) => x + y))
    }

    it("Right list shorter") {
      val l1 = 8 :: 7 ::Nil
      val l2 = 1 :: 2 :: 4 :: 6 :: Nil
      val expected = 9 :: 9 :: 4 :: 6 :: Nil
      assertListCmp(expected, ListOps.zipWith(l2, l1)((x, y) => x + y))
    }

    it("Equal lists") {
      val l1 = 8 :: 7 :: 0 :: 2 :: Nil
      val l2 = 1 :: 2 :: 4 :: 6 :: Nil
      val expected = 9 :: 9 :: 4 :: 8 :: Nil
      assertListCmp(expected, ListOps.zipWith(l1, l2)((x, y) => x + y))
    }
  }

  // 3.24
  describe("Has subsequence?") {
    it("Empty seq has empty subseq") {
      assert(true == ListOps.hasSubsequence(List[Int](), List[Int]()))
    }

    it("Any other seq has empty subseq") {
      assert(true == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, List[Int]()))
    }

    it("Seq has 1-elem subseq") {
      assert(true == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 1 :: Nil))
      assert(true == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 2 :: Nil))
      assert(true == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 3 :: Nil))
    }

    it("Seq has 2-elem subseq") {
      assert(true == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil))
      assert(true == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 2 :: 3 :: Nil))
    }

    it("Seq has 2-elem subseq, elem happens earlier as well") {
      assert(true == ListOps.hasSubsequence(2 :: 2 :: 3 :: Nil, 2 :: 3 :: Nil))
    }

    it("If chars are OK, but there's something in between, it's not a subseq") {
      assert(false == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 1 :: 3 :: Nil))
    }

    it("If chars are OK, but the order is wrong, it's not a subseq") {
      assert(false == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 2 :: 1 :: Nil))
    }

    it("If seq is equal subseq, that's ok") {
      assert(true == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil))
    }

    it("If subseq is wider than seq, it's not ok") {
      assert(false == ListOps.hasSubsequence(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: 4 :: Nil))
    }
  }

  // 3.25
  describe("Tree operations - size") {
    it("Single leaf") {
      assert(1 == ListOps.size(Leaf(1)))
    }

    it("One level") {
      assert(3 == ListOps.size(Branch(Leaf(1), Leaf(2))))
    }

    it("Two levels - left empty") {
      assert(5 == ListOps.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
    }

    it("Two levels - right empty") {
      assert(5 == ListOps.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    }

    it("Two levels - all filled") {
      assert(7 == ListOps.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    }

    it("Three levels") {
      assert(15 == ListOps.size(Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Branch(Leaf(5), Leaf(6)), Branch(Leaf(7), Leaf(8))))))
    }
  }

  // 3.26
  describe("Tree operations - maximum") {
    it("Single leaf") {
      assert(10 == ListOps.maximum(Leaf(10)))
    }

    it("One level") {
      assert(2 == ListOps.maximum(Branch(Leaf(1), Leaf(2))))
    }

    it("Two levels - left empty, max in top") {
      assert(10 == ListOps.maximum(Branch(Leaf(10), Branch(Leaf(2), Leaf(3)))))
    }

    it("Two levels - left empty, max in bottom") {
      assert(20 == ListOps.maximum(Branch(Leaf(10), Branch(Leaf(2), Leaf(20)))))
    }

    it("Two levels - right empty, max in top") {
      assert(10 == ListOps.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))))
    }

    it("Two levels - right empty, max in bottom") {
      assert(20 == ListOps.maximum(Branch(Branch(Leaf(20), Leaf(2)), Leaf(10))))
    }

    it("Two levels - all filled") {
      assert(4 == ListOps.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    }

    it("Three levels") {
      assert(9 == ListOps.maximum(Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Branch(Leaf(9), Leaf(6)), Branch(Leaf(7), Leaf(8))))))
    }
  }

  // 3.27
  describe("Tree operations - depth") {
    it("Single leaf") {
      assert(1 == ListOps.depth(Leaf(10)))
    }

    it("One level") {
      assert(2 == ListOps.depth(Branch(Leaf(1), Leaf(2))))
    }

    it("Two levels - left empty") {
      assert(3 == ListOps.depth(Branch(Leaf(10), Branch(Leaf(2), Leaf(3)))))
    }

    it("Two levels - right empty") {
      assert(3 == ListOps.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))))
    }

    it("Two levels - all filled") {
      assert(3 == ListOps.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    }

    it("Three levels") {
      assert(4 == ListOps.depth(Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Branch(Leaf(9), Leaf(6)), Branch(Leaf(7), Leaf(8))))))
    }
  }

  // 3.28
  private def treeCmp[A](t1: Tree[A], t2: Tree[A]): Boolean = {
    t1 match {
      case Leaf(a) => t2 match {
        case Leaf(`a`) => true
        case Leaf(_) => false
        case Branch(_, _) => false
      }
      case Branch(a, b) => t2 match {
        case Leaf(_) => false
        case Branch(c, d) => treeCmp(a, c) && treeCmp(b, d)
      }
    }
  }

  describe("Tree operations - map") {
    it("Compare identical leaves") {
      assert(true == treeCmp(Leaf(4), Leaf(4)))
    }

    it("Compare different leaves") {
      assert(false == treeCmp(Leaf(4), Leaf(3)))
    }

    it("Compare identical branches") {
      assert(true == treeCmp(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2))))
    }

    it("Compare different branches") {
      assert(false == treeCmp(Branch(Leaf(1), Leaf(2)), Branch(Leaf(2), Leaf(1))))
    }

    it("Compare different branches - diff composure") {
      val one = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
      val two = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

      assert(false == treeCmp(one, two))
    }

    it("Compare identical branches - irregular") {
      val one = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
      val two = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
      assert(true == treeCmp(one, two))
    }

    it("Compare different branches - irregular") {
      val one = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
      val two = Branch(Branch(Leaf(1), Leaf(4)), Leaf(3))
      assert(false == treeCmp(one, two))
    }

    it("Multiply by 2") {
      val input = Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))
      val expected = Branch(Branch(Leaf(2), Leaf(4)), Leaf(20))
      def func = (x: Int) => 2 * x
      assert(true == treeCmp(ListOps.map(input)(func), expected))
    }

    it("Subtract 3") {
      val input = Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))
      val expected = Branch(Branch(Leaf(-2), Leaf(-1)), Leaf(7))
      def func = (x: Int) => x - 3
      assert(true == treeCmp(ListOps.map(input)(func), expected))
    }
  }

  // 3.29
  describe("2: Tree operations - size") {
    it("Single leaf") {
      assert(1 == ListOps.size2(Leaf(1)))
    }

    it("One level") {
      assert(3 == ListOps.size2(Branch(Leaf(1), Leaf(2))))
    }

    it("Two levels - left empty") {
      assert(5 == ListOps.size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
    }

    it("Two levels - right empty") {
      assert(5 == ListOps.size2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    }

    it("Two levels - all filled") {
      assert(7 == ListOps.size2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    }

    it("Three levels") {
      assert(15 == ListOps.size2(Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Branch(Leaf(5), Leaf(6)), Branch(Leaf(7), Leaf(8))))))
    }
  }

  describe("2: Tree operations - maximum") {
    it("Single leaf") {
      assert(10 == ListOps.maximum2(Leaf(10)))
    }

    it("One level") {
      assert(2 == ListOps.maximum2(Branch(Leaf(1), Leaf(2))))
    }

    it("Two levels - left empty, max in top") {
      assert(10 == ListOps.maximum2(Branch(Leaf(10), Branch(Leaf(2), Leaf(3)))))
    }

    it("Two levels - left empty, max in bottom") {
      assert(20 == ListOps.maximum2(Branch(Leaf(10), Branch(Leaf(2), Leaf(20)))))
    }

    it("Two levels - right empty, max in top") {
      assert(10 == ListOps.maximum2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))))
    }

    it("Two levels - right empty, max in bottom") {
      assert(20 == ListOps.maximum2(Branch(Branch(Leaf(20), Leaf(2)), Leaf(10))))
    }

    it("Two levels - all filled") {
      assert(4 == ListOps.maximum2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    }

    it("Three levels") {
      assert(9 == ListOps.maximum2(Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Branch(Leaf(9), Leaf(6)), Branch(Leaf(7), Leaf(8))))))
    }
  }

  describe("2: Tree operations - depth") {
    it("Single leaf") {
      assert(1 == ListOps.depth2(Leaf(10)))
    }

    it("One level") {
      assert(2 == ListOps.depth2(Branch(Leaf(1), Leaf(2))))
    }

    it("Two levels - left empty") {
      assert(3 == ListOps.depth2(Branch(Leaf(10), Branch(Leaf(2), Leaf(3)))))
    }

    it("Two levels - right empty") {
      assert(3 == ListOps.depth2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))))
    }

    it("Two levels - all filled") {
      assert(3 == ListOps.depth2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
    }

    it("Three levels") {
      assert(4 == ListOps.depth2(Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Branch(Branch(Leaf(9), Leaf(6)), Branch(Leaf(7), Leaf(8))))))
    }
  }

  describe("2: Tree operations - map") {
    it("Multiply by 2") {
      val input = Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))
      val expected = Branch(Branch(Leaf(2), Leaf(4)), Leaf(20))
      def func = (x: Int) => 2 * x
      assert(true == treeCmp(ListOps.map2(input)(func), expected))
    }

    it("Subtract 3") {
      val input = Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))
      val expected = Branch(Branch(Leaf(-2), Leaf(-1)), Leaf(7))
      def func = (x: Int) => x - 3
      assert(true == treeCmp(ListOps.map2(input)(func), expected))
    }
  }
}
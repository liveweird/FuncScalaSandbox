package net.gebski.FuncScalaSandbox.Chapter2

import org.scalatest.FunSpec
import scala.util.Random

class IsSortedSpec extends FunSpec {

  val ascInt = (a: Int, b: Int) => a <= b

  describe("Is array sorted") {
    it("doesn't break on empty array") {
      assert(true == IsSorted.isSorted(Array[Int](), ascInt))
    }

    it("says that 1-elem array is sorted") {
      assert(true == IsSorted.isSorted(Array[Int](1), ascInt))
    }

    it("says that 2-elem array with sorted elems is sorted") {
      assert(true == IsSorted.isSorted(Array[Int](1, 2), ascInt))
    }

    it("says that 2-elem array with equal elems is sorted") {
      assert(true == IsSorted.isSorted(Array[Int](2, 2), ascInt))
    }

    it("says that 2-elem array with not sorted elems is not sorted") {
      assert(false == IsSorted.isSorted(Array[Int](2, 1), ascInt))
    }

    it("says that 5-elem array with sorted elems is sorted") {
      assert(true == IsSorted.isSorted(Array[Int](1, 2, 3, 4, 5), ascInt))
    }

    it("says that 5-elem array with 1 unsorted elem is not sorted") {
      assert(false == IsSorted.isSorted(Array[Int](2, 3, 1, 4, 5), ascInt))
    }

    it("says that 100-elem long random array that has been sorted is actually sorted") {
      assert(true == IsSorted.isSorted(Seq.fill(100)(Random.nextInt).toArray[Int].sortWith(ascInt), ascInt))
    }
  }
}

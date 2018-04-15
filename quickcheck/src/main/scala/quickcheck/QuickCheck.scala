package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(v, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert2findMin") = forAll { (v1: Int, v2: Int) =>
    val heap = insert(v2, insert(v1, empty))
    val smallest = Math.min(v1, v2)
    findMin(heap) == smallest
  }

  property("insertDeleteEmpty") = forAll { v: Int =>
    val heap = insert(v, empty)
    val emptyHeap = deleteMin(heap)
    emptyHeap == empty
  }

  property("sorted") = forAll { h: H =>
    def makeSorted(heap: H, list: List[Int]): List[Int] = {
      if (isEmpty(heap)) list
      else findMin(heap) :: makeSorted(deleteMin(heap), list)
    }
    val sortedList = makeSorted(h, Nil)
    sortedList == sortedList.sorted
  }

  property("meldedMin") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val meldedHeap = meld(h1, h2)
    val minFromMelded = findMin(meldedHeap)
    minFromMelded == min1 || minFromMelded == min2
  }

  // Given 2 heaps. Check that second min element from melded heaps equals second element of sorted list, which
  // is concatenation of lists of these heaps
  property("sorted2") = forAll { (h1: H, h2: H) =>
    def makeSorted(heap: H, list: List[Int]): List[Int] = {
      if (isEmpty(heap)) list
      else findMin(heap) :: makeSorted(deleteMin(heap), list)
    }
    val sortedList1 = makeSorted(h1, Nil)
    val sortedList2 = makeSorted(h2, Nil)

    val meldedHeap = meld(h1, h2)

    val twoSortedLists = (sortedList1 ++ sortedList2).sorted

    twoSortedLists.tail.head == findMin(deleteMin(meldedHeap))
  }
}

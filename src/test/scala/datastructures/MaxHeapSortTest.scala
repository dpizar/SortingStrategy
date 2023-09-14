package datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MaxHeapSortTest extends AnyFlatSpec with Matchers {
  behavior of "Getting left child in a heap"

  it should "return the correct index for the left child" in {
    val maxHeapStructure: Array[Int] = Array(14, 9, 8, 7, 2, 1)
    val maxHeapSort: MaxHeapSort = new MaxHeapSort(maxHeapStructure)

    val leftChildIndex  = maxHeapSort.leftChildIndex(1)
    val rightChildIndex = maxHeapSort.rightChildIndex(1)
    val parentIndex = maxHeapSort.parentIndex(1)

    3 should equal(leftChildIndex)
    4 should equal(rightChildIndex)
    0 should equal(parentIndex)
  }

  behavior of "MaxHeapify function"

  it should "maintain the heap property when run on root node, biggest value in left nodes" in {
    val maxHeapStructure: Array[Int] = Array(1, 9, 8, 7, 5, 3)
    val maxHeapSort: MaxHeapSort = new MaxHeapSort(maxHeapStructure)

    maxHeapSort.maxHeapify(0)

    Array(9, 7, 8, 1, 5, 3) should equal(maxHeapSort.toSort)
  }

  it should "maintain the heap property when run on root node, biggest value in right nodes" in {
    val maxHeapStructure: Array[Int] = Array(1, 8, 9, 5, 7, 3, 4)
    val maxHeapSort: MaxHeapSort = new MaxHeapSort(maxHeapStructure)

    maxHeapSort.maxHeapify(0)

    Array(9, 8, 4, 5, 7, 3, 1) should equal(maxHeapSort.toSort)
  }

  it should "maintain the hep property when run on leave node" in {
    val maxHeapStructure: Array[Int] = Array(14, 9, 8, 7, 5, 23)
    val maxHeapSort: MaxHeapSort = new MaxHeapSort(maxHeapStructure)

    maxHeapSort.maxHeapify(4)

    Array(14, 9, 8, 7, 5, 23) should equal(maxHeapSort.toSort)
  }

}

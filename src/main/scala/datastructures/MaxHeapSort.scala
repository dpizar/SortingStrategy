package datastructures

import scala.annotation.tailrec

class MaxHeapSort(val toSort: Array[Int]) {
  private var variableHeapSize: Int = toSort.length
  def heapSort(): List[Int] = {
    variableHeapSize = toSort.length
    buildMaxHeap()
    for (i <- (1 until toSort.length).reverse) {
      swap(0, i)
      variableHeapSize -= 1
      maxHeapify(0)
    }
    toSort.toList
  }
  private def buildMaxHeap(): Unit = {
    val numInternalNodes = Math.floor(heapSize / 2).toInt
    for (i <- (0 until numInternalNodes).reverse)
      maxHeapify(i)
  }
  @tailrec
  private def maxHeapify(index: Int): Unit = {
    val left = leftChildIndex(index)
    val right = rightChildIndex(index)

    var largest = index
    if (left < heapSize && toSort(left) > toSort(index))
      largest = left
    if (right < heapSize && toSort(right) > toSort(largest))
      largest = right

    if (largest != index) {
      swap(index, largest)
      maxHeapify(largest)
    }
  }
  private def leftChildIndex(index: Int): Int = (2 * index) + 1
  private def rightChildIndex(index: Int): Int = (2 * index) + 2
  private def heapSize: Int = variableHeapSize
  private def swap(firstIndex: Int, secondIndex: Int): Unit = {
    val temp = toSort(firstIndex)
    toSort(firstIndex) = toSort(secondIndex)
    toSort(secondIndex) = temp
  }
  private def parentIndex(index: Int): Int = Math.floor(index / 2).toInt
}

object MaxHeapSort {
  def apply(toSort: List[Int]): MaxHeapSort = {
    val arrToSort = new Array[Int](toSort.length)
    val _ = toSort.copyToArray(arrToSort)
    new MaxHeapSort(arrToSort)
  }
}

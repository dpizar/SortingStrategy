package sorting

import datastructures.MaxHeapSort
import merge.MergingTwoListsBehaviour
import utilities.ListOperations.swapValuesInArray

import scala.collection.immutable
import scala.util.Random

trait SortingBehaviour {
  def sort(toSort: List[Int]): List[Int]
}

class InsertionSort extends SortingBehaviour {
  def sort(toSort: List[Int]): List[Int] = {
    val arr = new Array[Int](toSort.length)
    val _ = toSort.copyToArray(arr)
    sort(arr)
  }

  def sort(toSort: Array[Int]): List[Int] = {
    for {
      currentKey <- 1 to toSort.length
      j <- List.range(1, currentKey).reverse
      if toSort(j - 1) > toSort(j)
    }
      swapValuesInArray(toSort, j, j - 1)
    toSort.toList
  }
}

class InsertionSortRecursive extends SortingBehaviour{
  /**
   * For implementing this algorithm, we need a function which inserts an element in the already sorted list
   */
  def sort(toSort: List[Int]): List[Int] =
    if (toSort.isEmpty) Nil
    else {
      val sorted = sort(toSort.tail)
      insert(toSort.head, sorted)
    }

  /**
   * The second problem is inserting the head into the correct place which this function solves.
   */
  private def insert(x: Int, xs: List[Int]): List[Int] =
    if (xs.isEmpty || x <= xs.head) x :: xs
    else xs.head :: insert(x, xs.tail)
}

class MergeSort(val combineTwoList: MergingTwoListsBehaviour) extends SortingBehaviour {
  def sort(toSort: List[Int]): List[Int] = {
    if (toSort.length <= 1) // <= in case we send an empty list
      toSort
    else{
      val (leftSideToSort, rightSideToSort)= splitByHalf(toSort)
      val leftSide = sort(leftSideToSort)
      val rightSide = sort(rightSideToSort)
      combineTwoList.mergeTwoSortedLists(leftSide, rightSide)
    }
  }

  private def splitByHalf(toSplit: List[Int]): (List[Int], List[Int]) = {
    val middle: Int = Math.ceil(toSplit.length / 2).toInt
    toSplit.splitAt(middle)
  }
}

class HeapSort extends SortingBehaviour {
  def sort(toSort: List[Int]): List[Int] = {
    val maxHeapSort = MaxHeapSort(toSort)
    maxHeapSort.heapSort()
  }
}

class QuickSort extends SortingBehaviour {
  def sort(toSort: List[Int]): List[Int] = {
    val arr = new Array[Int](toSort.length)
    val _ = toSort.copyToArray(arr)
    quickSort(arr, 0, toSort.length - 1)
    arr.toList
  }

  def quickSort(toSort: Array[Int], startIndex: Int, endIndex: Int): Unit =
    if (startIndex < endIndex){
      val pivotIndex = randomPartition(toSort, startIndex, endIndex)
      quickSort(toSort, startIndex, pivotIndex - 1)
      quickSort(toSort, pivotIndex + 1, endIndex)
    }

  private def randomPartition(toSort: Array[Int], startIndex: Int, endIndex: Int): Int = {
    val randomIndex = Random.between(startIndex, endIndex + 1)
    swapValuesInArray(toSort, randomIndex, endIndex)
    val pivot = toSort(endIndex)

    var partitionIndex = startIndex - 1 // start at -1
    for(
      j <- startIndex until endIndex
      if toSort(j) <= pivot
    ) {
      partitionIndex += 1
      swapValuesInArray(toSort, partitionIndex, j)
    }
    swapValuesInArray(toSort, partitionIndex + 1, endIndex)
    partitionIndex + 1
  }
}

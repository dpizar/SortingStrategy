package sorting

import scala.collection.immutable

trait SortingBehaviour {
  def sort(toSort: List[Int]): List[Int]
}

class InsertionSort extends SortingBehaviour {

  def sort(toSort: List[Int]): List[Int] = {
    val arr = new Array[Int](toSort.length)
    val copied = toSort.copyToArray(arr)
    sort(arr)
  }

  def sort(toSort: Array[Int]): List[Int] = {
    for {
      currentKey <- 1 to toSort.length
      j <- List.range(1, currentKey).reverse
      if toSort(j - 1) > toSort(j)
    }
        swap(toSort, j, j - 1)
    toSort.toList
  }

  private def swap(toSort: Array[Int], i: Int, j: Int): Unit = {
    val temp = toSort(i)
    toSort(i) = toSort(j)
    toSort(j) = temp
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

class MergeSort extends SortingBehaviour {
  def sort(toSort: List[Int]): List[Int] = {
    if (toSort.length <= 1) // <= in case we send an empty list
      toSort
    else{
      val (leftSideToSort, rightSideToSort)= splitByHalf(toSort)
      val leftSide = sort(leftSideToSort)
      val rightSide = sort(rightSideToSort)
      combineOrderedLists(leftSide, rightSide)
    }
  }

  private def splitByHalf(toSplit: List[Int]): (List[Int], List[Int]) = {
    val middle: Int = Math.ceil(toSplit.length / 2).toInt
    toSplit.splitAt(middle)
  }

  private def combineOrderedLists(leftSide: List[Int], rightSide: List[Int]): List[Int] =
    (leftSide, rightSide) match {
      case (Nil, Nil) => Nil
      case (x::xs, Nil) => leftSide
      case (Nil, x::xs) => rightSide
      case (x::xs, y::ys) =>
        if (x <= y)
          x :: combineOrderedLists(xs, rightSide)
        else
          y :: combineOrderedLists(leftSide, ys)
    }

  private def combineOrderedListDeterministic(leftSide: List[Int], rightSide: List[Int]): List[Int] = {
    val totalLength: Int = leftSide.length + rightSide.length
    val combinedList: Array[Int] = new Array[Int](totalLength)
    var i, j = 0
    for (currentCount <- 0 until totalLength) {
      val currentLeftSide = if (i < leftSide.length) leftSide(i) else 1000000
      val currentRightSide = if (j < rightSide.length) rightSide(j) else 1000000
      combinedList(currentCount) =
        if (currentLeftSide <= currentRightSide) {
          i += 1
          leftSide(i - 1)
        } else {
          j += 1
          rightSide(j - 1)
        }
    }
    combinedList.toList
  }
}

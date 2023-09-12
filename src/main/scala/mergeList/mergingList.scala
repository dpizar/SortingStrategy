package mergeList

trait MergingTwoListsBehaviour {
  def mergeTwoSortedLists(leftSide: List[Int], rightSide: List[Int]): List[Int]
}

class MergeTwoListDeterministic extends MergingTwoListsBehaviour {
  def mergeTwoSortedLists(leftSide: List[Int], rightSide: List[Int]): List[Int] ={
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

class MergeTwoListRecursively extends MergingTwoListsBehaviour {
  def mergeTwoSortedLists(leftSide: List[Int], rightSide: List[Int]): List[Int] =
    (leftSide, rightSide) match {
      case (Nil, Nil) => Nil
      case (x :: xs, Nil) => leftSide
      case (Nil, x :: xs) => rightSide
      case (x :: xs, y :: ys) =>
        if (x <= y)
          x :: mergeTwoSortedLists(xs, rightSide)
        else
          y :: mergeTwoSortedLists(leftSide, ys)
    }
}


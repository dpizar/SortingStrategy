package sorting

import mergeList.MergeTwoListRecursively

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait OneIntegerSequence {
  val unorderedOneValue: List[Int] = List(9)
  val orderedOneValue: List[Int] = List(9)
}

trait MultipleIntegerSequence {
  val unorderedMultipleValues: List[Int] = List(9, 5, 7, 11, 2, 6)
  val orderedMultipleValues: List[Int] = List(2, 5, 6, 7, 9, 11)
}

trait ReverseOrderedIntegerSequence {
  val unorderedReversedIntegerValues: List[Int] = List(55, 48, 36, 24, 14, 9, 7, 6, 3, 1)
  val orderedReversedIntegerValues: List[Int] = List(1, 3, 6, 7, 9, 14, 24, 36, 48, 55)
}

trait SortingBehaviourTest extends AnyFlatSpec with Matchers {
  def sortingBehaviourTest(sortingAlgorithm: SortingBehaviour): Unit = {
    it should "sort various unordered integers in ascending order" in new MultipleIntegerSequence {
      val ordered: List[Int] = sortingAlgorithm.sort(unorderedMultipleValues)

      ordered should equal(orderedMultipleValues)
    }

    it should "sort one integer" in new OneIntegerSequence {
      val ordered: List[Int] = sortingAlgorithm.sort(unorderedOneValue)

      ordered should equal(orderedOneValue)
    }

    it should "sort in ascending order a sequence that is sorted in descending order" in new ReverseOrderedIntegerSequence {
      val ordered: List[Int] = sortingAlgorithm.sort(unorderedReversedIntegerValues)

      ordered should equal(orderedReversedIntegerValues)
    }

    it should "return empty list if initial list is empty" in {
      val ordered: List[Int] = sortingAlgorithm.sort(List.empty)

      ordered should equal(List.empty)
    }
  }
}

class InsertionSortTest extends SortingBehaviourTest {
  val insertionSort: InsertionSort = InsertionSort()

  behavior of "Insertion Sort Algorithm"

  it should behave like sortingBehaviourTest(insertionSort)
}

class MergeSortTest extends SortingBehaviourTest {
  val mergeSort: MergeSort = MergeSort(new MergeTwoListRecursively)

  behavior of "Merge Sort Algorithm"

  it should behave like sortingBehaviourTest(mergeSort)
}

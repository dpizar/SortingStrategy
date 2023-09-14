package merge

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


trait MergingTwoListsBehaviourTest extends AnyFlatSpec with Matchers {
  def mergingListBehaviourTest(mergingListAlgorithm: MergingTwoListsBehaviour): Unit = {
    it should "merge in ascending order two sorted lists" in {
      val leftList = List(2, 4, 6, 8, 10)
      val rightList = List(1, 3, 5, 7 , 9)

      val mergedList: List[Int] = mergingListAlgorithm.mergeTwoSortedLists(leftList, rightList)

      mergedList should equal(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    }

    it should "return empty list if lists are empty" in {
      val leftList = List.empty
      val rightList = List.empty

      val mergedList: List[Int] = mergingListAlgorithm.mergeTwoSortedLists(leftList, rightList)

      mergedList should equal(List.empty)
    }

    it should "return right list if left list is empty" in {
      val leftList = List.empty
      val rightList = List(2, 8, 9, 55)

      val mergedList: List[Int] = mergingListAlgorithm.mergeTwoSortedLists(leftList, rightList)

      mergedList should equal(rightList)
    }

    it should "return the left list if right list is empty" in {
      val leftList = List(2, 8, 9, 55)
      val rightList = List.empty

      val mergedList: List[Int] = mergingListAlgorithm.mergeTwoSortedLists(leftList, rightList)

      mergedList should equal(leftList)
    }
  }
}

class MergeTwoListDeterministicTest extends MergingTwoListsBehaviourTest {
  val mergeTwoListDeterministic: MergingTwoListsBehaviour = new MergeTwoListDeterministic

  behavior of "Deterministic merging of two lists"

  it should behave like mergingListBehaviourTest(mergeTwoListDeterministic)
}

class MergeTwoListRecursivelyTest extends MergingTwoListsBehaviourTest {
  val mergeTwoListRecursively: MergingTwoListsBehaviour = new MergeTwoListRecursively

  behavior of "Recursively merging of two lists"

  it should behave like mergingListBehaviourTest(mergeTwoListRecursively)
}


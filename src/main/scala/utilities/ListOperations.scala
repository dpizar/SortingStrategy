package utilities

object ListOperations {
  def swapValuesInArray(arrayToSwap: Array[Int], indexOne: Int, indexTwo: Int): Unit = {
    val temp = arrayToSwap(indexOne)
    arrayToSwap(indexOne) = arrayToSwap(indexTwo)
    arrayToSwap(indexTwo) = temp
  }
}

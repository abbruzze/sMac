package ucesoft.mac.debugger

/**
 * Knuth-Morris-Pratt (KMP) algo.
 * @author Chat-GPT
 *         Created on 16/01/2025 15:14  
 */
object ByteArraySearch {
  def findSubArrayIndex(arr: Array[Int], subArr: Array[Int]): Option[Int] = {
    if (subArr.isEmpty) return Some(0)
    if (arr.length < subArr.length) return None

    val failure = buildFailureTable(subArr)

    var i = 0
    var j = 0

    while (i < arr.length) {
      if (arr(i) == subArr(j)) {
        i += 1
        j += 1
        if (j == subArr.length) return Some(i - j)
      } else if (j > 0) {
        j = failure(j - 1)
      } else {
        i += 1
      }
    }
    None
  }

  private def buildFailureTable(pattern: Array[Int]): Array[Int] = {
    val failure = Array.fill(pattern.length)(0)
    var j = 0

    var i = 1
    while (i < pattern.length) {
      if (pattern(i) == pattern(j)) {
        j += 1
        failure(i) = j
        i += 1
      } else if (j > 0) {
        j = failure(j - 1)
      } else {
        failure(i) = 0
        i += 1
      }
    }
    failure
  }
}

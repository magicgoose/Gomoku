package magicgoose.sorting

import scala.{ specialized => spec }
import scala.util.Random

object QuickSort2 {
  //Dummy objects are for workaround scala specialization bug
  import SimpleSort2.Dummy0.swap
  import Dummy1._
  /**
   * sort 2 arrays based on values in second array
   */
  def sortInPlace[@spec A, @spec B](x: Array[A], y: Array[B], left: Int, right: Int)(implicit ar: Ord[B]) = {
    quicksort(x, y, left, right)
  }
  private object Dummy0 {
    @inline def partition[@spec A, @spec B](x: Array[A], y: Array[B], left: Int, right: Int, pivotIndex: Int)(implicit ar: Ord[B]) = {
      val pivotValue = y(pivotIndex)
      swap(x, y, pivotIndex, right) // Move pivot to end
      var storeIndex = left
      var i = left
      while (i < right) {
        if (ar.lt(y(i), pivotValue)) {
          swap(x, y, i, storeIndex)
          storeIndex += 1
        }
        i += 1
      }
      swap(x, y, storeIndex, right) // Move pivot to its final place
      storeIndex
    }
  }
  import Dummy0._
  private object Dummy1 {
    @inline def quicksort[@spec A, @spec B](x: Array[A], y: Array[B], left: Int, right: Int)(implicit ar: Ord[B]): Unit = {
      if (left < right) {
        if (right - left < 5) SimpleSort2.sortInPlace(x, y, left, right)
        else {
          val pivotIndex = left + Random.nextInt(right - left + 1)
          val pivotNewIndex = partition(x, y, left, right, pivotIndex)
          quicksort(x, y, left, pivotNewIndex - 1)
          quicksort(x, y, pivotNewIndex + 1, right)
        }
      }
    }
  }
}

object SimpleSort2 {
  object Dummy0 {
    @inline def swap[@spec A, @spec B](x: Array[A], y: Array[B], i1: Int, i2: Int) = {
      val tmp = x(i1)
      x(i1) = x(i2)
      x(i2) = tmp
      val tmp2 = y(i1)
      y(i1) = y(i2)
      y(i2) = tmp2
    }
  }
  import Dummy0._
  private object Dummy1 {
    def sortInPlace[@spec A, @spec B](x: Array[A], y: Array[B], left: Int, right: Int)(implicit ar: Ord[B]) = {
      var i1 = left
      while (i1 <= right) {
        var i2 = i1 + 1
        while (i2 <= right) {
          if (ar.lt(y(i2), y(i1))) {
            swap(x, y, i1, i2)
          }
          i2 += 1
        }
        i1 += 1
      }
    }
  }
  // sort 2 arrays based on values in second array
  def sortInPlace[@spec A, @spec B](x: Array[A], y: Array[B], left: Int, right: Int)(implicit ar: Ord[B]) =
    Dummy1.sortInPlace(x, y, left, right)
}

package magicgoose.sorting

import scala.{ specialized => spec }
import scala.util.Random

object QuickSort2 {
  
//  def main(args: Array[String]): Unit = {
//    while(true) {
//      val a1 = readLine().split(" ").map(_.toInt)
//      val a2 = readLine().split(" ").map(_.toInt)
//      val maxlen = a1.length max a2.length
//      import NaturalOrd._
//      QuickSort2.sortInPlace(a1, a2, 0, maxlen - 1)
//      println(a1.mkString(" "))
//      println(a2.mkString(" "))
//      
//    }
//    
//  }
  
  
  import SimpleSort2.Suck0.swap
  import Suck1._
  /**
   * sort 2 arrays based on values in second array
   */
  def sortInPlace[@spec A, @spec B](x: Array[A], y: Array[B], left: Int, right: Int)(implicit ar: Ord[B]) = {
    quicksort(x, y, left, right)
  }
  private object Suck0 {
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
  import Suck0._
  private object Suck1 {
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
  object Suck0 {
    @inline def swap[@spec A, @spec B](x: Array[A], y: Array[B], i1: Int, i2: Int) = {
      val tmp = x(i1)
      x(i1) = x(i2)
      x(i2) = tmp
      val tmp2 = y(i1)
      y(i1) = y(i2)
      y(i2) = tmp2
    }
  }
  import Suck0._
  private object Suck1 {
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
    Suck1.sortInPlace(x, y, left, right)
}
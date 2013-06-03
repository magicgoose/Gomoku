package magicgoose.gomoku

import java.util.Arrays
import magicgoose.sorting.Ord
//import magicgoose.sorting.DualPivotQuicksortGenSingleton
package object ai {
  import scala.reflect.ClassTag

  def perfectSquareRoot(n: Int) = {
    val h = n & 0xF
    var t = 0
    if (!(h > 9) && (h != 2 && h != 3 && h != 5 && h != 6 && h != 7 && h != 8) && {
      t = math.floor(Math.sqrt(n.toDouble) + 0.5).toInt
      t * t == n
    }) t
    else -1
  }

  def dot(x: Array[Int], y: Array[Int]): Int = {
    val l = x.length
    assert(l == y.length)
    var r = 0
    var i = 0
    while (i < l) {
      r += (x(i) * y(i))
      i += 1
    }
    r
  }

  def array_tabulate[@specialized T: ClassTag](start: Int, end: Int, step: Int, fun: Int => T) = {
    val length = (end - start + (step - 1)) / step
    val r = Array.ofDim[T](length)
    var i = 0
    var a = start
    while (i < length) {
      r(i) = fun(a)
      i += 1
      a += step
    }
    r
  }
  @inline final def array_foreach[@specialized T](x: Array[T])(fun: T => Unit) {
    var i = 0
    while (i < x.length) {
      fun(x(i))
      i += 1
    }
  }

  def pack(a: Int, b: Int): Int = {
    ((a.toShort & 0xFFFF) | ((b.toShort & 0xFFFF) << 16))
  }
  def unpack1(x: Int): Int = {
    (x & 0xFFFF).toShort
  }
  def unpack2(x: Int): Int = {
    x >> 16
  }

  def isPrime(n: Int) = {
    n > 1 &&
      (2 until math.min(n, math.sqrt(n + 1).toInt + 1)).forall(y => (n % y) != 0)
  }
  def primesIter = new Iterator[Int] {
    var p = 1
    def hasNext = true
    def next(): Int = {
      val r = p
      p += 1
      while (!isPrime(p)) { p += 1 }
      r
    }
  }

  @inline final def packLong(x: Int, y: Int) = {
    val xPacked = (x.toLong) << 32
    val yPacked = y & 0xFFFFFFFFL
    xPacked | yPacked
  }

  @inline final def unpackX(packed: Long) = {
    ((packed >> 32) & 0xFFFFFFFFL).toInt
  }

  @inline final def unpackY(packed: Long) = {
    (packed & 0xFFFFFFFFL).toInt
  }
}
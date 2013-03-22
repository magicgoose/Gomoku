package magicgoose.gomoku

import java.util.Arrays
package object ai {
  import scala.reflect.ClassTag

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
    x  >> 16
  }
}
package magicgoose.gomoku

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

  def array_tabulate[@specialized(Int) T: ClassTag](start: Int, end: Int, step: Int, fun: Int => T) = {
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

  def pack(a: Byte, b: Byte): Short = {
    ((a & 0xFF) | ((b & 0xFF) << 8)).toShort
  }
  def unpack1(x: Short): Byte = {
    (x & 0xFF).toByte
  }
  def unpack2(x: Short): Byte = {
    ((x & 0xFFFF) >> 8).toByte
  }

}
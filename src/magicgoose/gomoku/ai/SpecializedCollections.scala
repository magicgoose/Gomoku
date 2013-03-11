package magicgoose.gomoku.ai

import scala.reflect.ClassTag
import scala.collection.immutable.VectorBuilder
import scala.annotation.tailrec
import scala.util.Random

trait Enumerable[@specialized(Int) T] {
  @inline def foreach(f: T => Unit)
  @inline def findIndex(f: T => Boolean): Int
  @inline final def foldLeft[@specialized(Int) R](init: R)(fun: (R, T) => R) = {
    var r = init
    foreach(e => r = fun(r, e))
    r
  }
  @inline final def contains(f: T => Boolean) = {
    findIndex(f) != -1
  }
  @inline final def maxBy(init: T)(fn: T => Int): T = {
    var m = Int.MinValue
    var r = init
    foreach(e => {
      val f = fn(e)
      if (f > m) {
        r = e
        m = f
      }
    })
    r
  }
  @inline final def maxMap(fn: T => Int): Int = {
    var m = Int.MinValue
    foreach(e => {
      val f = fn(e)
      if (f > m) {
        m = f
      }
    })
    m
  }
}
trait Indexed[@specialized(Int) T] extends Enumerable[T] {
  @inline def length: Int
  @inline def apply(i: Int): T
  @inline def getRandom: T = apply(Random.nextInt(length))
  @inline final def foreach(f: T => Unit) {
    var i = 0
    while (i < length) {
      f(this(i))
      i += 1
    }
  }
  @inline final def findIndex(fun: T => Boolean) = {
    @tailrec def find(i: Int): Int = {
      if (i < length) {
        if (fun(this(i))) i
        else find(i + 1)
      } else -1
    }
    find(0)
  }
}


final class ArraySlice[@specialized(Int) T: ClassTag](private val begin: Int, private val end: Int, private val data: Array[T]) extends Indexed[T] {
  @inline final val length = end - begin
  @inline final def apply(i: Int) = { assert(i < length && i >= 0); data(i + begin) }
  @inline final def split(fun: T => Boolean) = {
    val r = GrowableArray.create[ArraySlice[T]](length)//new VectorBuilder[ArraySlice[T]]
    var begin = this.begin
    var caret = begin
    while (caret < this.end) {
      if (fun(data(caret))) {
        if (caret > begin) {
          r.push(new ArraySlice(begin, caret, data))
        }
        begin = caret + 1
      }
      caret += 1
    }
    if (caret > begin) {
      r.push(new ArraySlice(begin, caret, data))
    }
    r
  }
}

final object GrowableArray {
  @inline final def create[@specialized(Int) T: ClassTag](max_size: Int) = {
    new GrowableArray(Array.ofDim[T](max_size))
  }
}

final class GrowableArray[@specialized(Int) T: ClassTag] private (private val data: Array[T]) extends Indexed[T] {
  private var _length = 0
  @inline final def length = _length
  @inline final def reset() {
    _length = 0
  }
  @inline final def apply(i: Int) = {
    assert(i < length)
    data(i)
  }
  @inline final def push(x: T) {
    data(length) = x
    _length += 1
  }

  @inline final def getSlice = new ArraySlice(0, length, data)
}
package magicgoose.sorting

import scala.{ specialized => spec }

trait Ord[@spec T] {
  @inline def lt(a: T, b: T): Boolean
  @inline def lte(a: T, b: T): Boolean
//  @inline def gt(a: T, b: T): Boolean
//  @inline def gte(a: T, b: T): Boolean
//  @inline def inv(a: T): T
//  @inline val zero: T
//  @inline val one: T
//  @inline def add(a: T, b: T): T
//  @inline def sub(a: T, b: T): T
//  @inline def mul(a: T, b: T): T
//  @inline def div(a: T, b: T): T
//  @inline def intValue(a: Int): Int
}

final object NaturalOrd {
  implicit final object ordInt extends Ord[Int] {
    @inline final def lt(a: Int, b: Int) = a < b
    @inline final def lte(a: Int, b: Int) = a <= b
//    @inline final def gt(a: Int, b: Int) = a > b
//    @inline final def gte(a: Int, b: Int) = a >= b
//    @inline final def inv(a: Int) = -a
//    @inline final val zero: Int = 0
//    @inline final val one: Int = 1
//    @inline final def add(a: Int, b: Int) = a + b
//    @inline final def sub(a: Int, b: Int) = a - b
//    @inline final def mul(a: Int, b: Int) = a * b
//    @inline final def div(a: Int, b: Int) = a / b
//    @inline final def intValue(a: Int): Int = a
  }
  // for example
  implicit final object ordFloat extends Ord[Float] {
    @inline final def lt(a: Float, b: Float) = a < b
    @inline final def lte(a: Float, b: Float) = a <= b
//    @inline final def gt(a: Float, b: Float) = a > b
//    @inline final def gte(a: Float, b: Float) = a >= b
  }
}

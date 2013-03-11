package magicgoose.gomoku

package object gui {
  def perfectSquareRoot(n: Int) = {
    val h = n & 0xF
    var t = 0
    if (!(h > 9) && (h != 2 && h != 3 && h != 5 && h != 6 && h != 7 && h != 8) && {
      t = math.floor(Math.sqrt(n.toDouble) + 0.5).toInt
      t * t == n
    }) t
    else -1
  }
  def foreach_i[@specialized(Int, Long) T](x: Array[T])(fun: (Int, T) => Unit) {
    var i = 0
    while (i < x.length) {
      fun(i, x(i))
      i += 1
    }
  }
  def format_exception(e: Throwable): String = {
    if (e == null) ""
    else
      e.toString() + "\n" +
        e.getStackTrace()
        .iterator
        .find(_.getClassName()
          .startsWith("magicgoose"))
        .getOrElse(e.getStackTraceString) +
        format_exception(e.getCause())
  }
}
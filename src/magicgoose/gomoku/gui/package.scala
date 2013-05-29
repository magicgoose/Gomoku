package magicgoose.gomoku

import java.io.FilenameFilter
import java.io.File
import javax.swing.filechooser.FileFilter
package object gui {

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

  implicit def fun2filenamefilter(x: (File => Boolean, String)) = {
    val (fun, descr) = x
    new FileFilter {
      override def accept(file: File) = {
        fun(file)
      }
      override def getDescription = descr
    }
  }

  def slurp(filename: String): String = { //obtain content of small text file
    var source: scala.io.BufferedSource = null
    try {
      source = scala.io.Source.fromFile(filename)
      source.mkString
    } finally {
      if (source != null) source.close()
    }
  }
}
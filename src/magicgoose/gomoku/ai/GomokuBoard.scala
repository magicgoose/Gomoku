package magicgoose.gomoku.ai

import java.util.Arrays

import scala.Array.canBuildFrom
import scala.language.implicitConversions

class GomokuBoard private (
  private val contents: Array[Int],
  val side_size: Int,
  val total_size: Int) {

  final val line_length = 5

  private val lines = { // position -> line number -> (stats, line indexes)
    val lines_- = {
      array_tabulate(0, side_size, 1, y =>
        Array.range(side_size * y, side_size * (y + 1), 1))
    }
    val lines_| = {
      array_tabulate(0, side_size, 1, x =>
        Array.range(x, x + total_size, side_size))
    }
    val lines_/ = {
      array_tabulate(line_length - 1, 2 * side_size - line_length, 1, slice => {
        val bound = math.max(0, slice - side_size + 1)
        Array.range(slice + (side_size - 1) * bound, bound + side_size * (slice - bound + 1) - 1, side_size - 1)
      })
    }
    val lines_\ = {
      array_tabulate(line_length - 1, 2 * side_size - line_length, 1, slice => {
        val bound = math.max(0, slice - side_size + 1)
        Array.range(bound + side_size * (side_size - 1 - slice + bound), slice - bound + side_size * (side_size - bound) + 1, 1 + side_size)
      })
    }
    val all_lines = Array.concat(lines_-, lines_|, lines_/, lines_\).map(l => (new LineInfo(), l))
    Array.tabulate(total_size)(c => {
      (all_lines.filter(_._2.contains(c)))
    })
  }
  val overall_line_info = new LineInfo()

  def reset_!() {
    lines.foreach(_.foreach(_._1.reset()))
    overall_line_info.reset()
    Arrays.fill(contents, 0)
  }
  
  private var current_player = 1
  def update_!(coord: Int)(new_value: Int) = {
    assert(new_value == 0 || (contents(coord) == 0 && new_value == current_player && overall_line_info.win == 0))
    contents(coord) = new_value
    current_player *= -1
    val crossing_lines = lines(coord)
    array_foreach(crossing_lines)(t => {
      val (line_info, line_indexes) = t
      overall_line_info -= line_info
      //println(s"before: $line_info")
      line_info.reset()
      eval_line_stats(line_indexes, line_info)
      overall_line_info += line_info
      //println(s"after: $line_info")
    })
  }

  def apply(coord: Int) = contents(coord)
  
  val neighbours2 = Array.tabulate(total_size)(i => {
    val x = i % side_size
    val y = i / side_size
    (for (
        xx <- (0 max (x - 2)) to ((side_size - 2) min (x + 2));
        yy <- (0 max (y - 2)) to ((side_size - 2) min (y + 2)))
      yield (xx + side_size * yy)).toArray
  })

  def canEqual(other: Any) = other.isInstanceOf[GomokuBoard]

  override def equals(other: Any) = {
    other match {
      case b: GomokuBoard => {
        b.canEqual(GomokuBoard.this) &&
          b.side_size == side_size &&
          Arrays.equals(b.contents, contents)
      }
      case _ => false
    }
  }
  override def hashCode() =
    Arrays.hashCode(contents)

  //  val cell_formatter = (x: Int) => {
  //    x match {
  //      case 1 => "+"
  //      case 0 => " "
  //      case -1 => "-"
  //    }
  //  }
  //  val hor_pad_length = side_size
  //
  //  override def toString() = {
  //    val hor_pad = "─" * hor_pad_length
  //    "┌" + hor_pad + "┐\n│" +
  //      contents.iterator.sliding(side_size, side_size).map(_.map(cell_formatter).mkString).mkString("│\n│") +
  //      "│\n└" + hor_pad + "┘\n"
  //
  //  }

  //Extra navigation functions

  private def eval_line_stats(line_indexes: Array[Int], result: LineInfo) = {
    eval_line_rle(line_indexes, rle_tmp)
    val wini = rle_tmp.findIndex(chunk => unpack1(chunk) != 0 && unpack2(chunk) >= line_length)
    if (wini != -1) {
      result.win = unpack1(rle_tmp(wini))
    } else {
      search_patterns(1, rle_tmp, result)
      search_patterns(-1, rle_tmp, result)
    }
  }

  private val rle_tmp = GrowableArray.create[Short](side_size)
  private def eval_line_rle(line_indexes: Array[Int], result: GrowableArray[Short]) { // write line stats for player into array
    rle_tmp.reset()
    var i = 0
    var last_type = 42 // Neither -1, 0, 1
    var last_count = 0
    while (i < line_indexes.length) {
      val current = contents(line_indexes(i))

      if (current != last_type) {
        if (last_type != 42) {
          result.push(pack(last_type.toByte, last_count.toByte))
        }
        last_type = current
        last_count = 1
      } else {
        last_count += 1
      }

      i += 1
    }
    result.push(pack(last_type.toByte, last_count.toByte))
  }

  private def search_patterns(player: Int, rle: GrowableArray[Short], result: LineInfo) = {
    val write_offset = (1 - player) / 2 * LineInfo.sz / 2
    def commit(size: Int, broken: Boolean, closed: Boolean) {
      if (size > 1) {
        val rs = math.min(4, size)
        import scala.language.implicitConversions
        @inline implicit def bool2int(b: Boolean) = if (b) 1 else 0
        result.patterns(write_offset + (rs - 2) * 4 + closed + (broken * 2)) += 1
      }

    }
    val windows = rle.getSlice.split(unpack1(_) == -player)
    windows.foreach(w => {
      val size = w.foldLeft(0)((acc, e) => unpack2(e) + acc)
      if (size >= line_length) {

        val first_index = if (unpack1(w(0)) == player) 0 else 1
        val last_index = if (unpack1(w(w.length - 1)) == player) w.length - 1 else w.length - 2
        assert((last_index - first_index) % 2 == 0)
        var l = first_index
        while (l <= (last_index - 2)) {
          assert(unpack1(w(l + 1)) == 0)
          val closed_l = (l == 0)
          if (unpack2(w(l + 1)) == 1) {
            val closed = closed_l || (l + 2 == w.length - 1)
            commit(unpack2(w(l)) + unpack2(w(l + 2)), true, closed)
            l += (if ((l + 3) < last_index && unpack2(w(l + 3)) == 1) 2 else 4)
          } else {
            commit(unpack2(w(l)), false, closed_l)
            l += 2
          }
        }
        if (l == last_index) commit(unpack2(w(l)), false, l == w.length - 1 || l == 0)
      }
    })
  }

}

object GomokuBoard {
  def empty(side_size: Int) = {
    val total_size = side_size * side_size
    new GomokuBoard(Array.fill(total_size)(0), side_size, total_size)
  }
}
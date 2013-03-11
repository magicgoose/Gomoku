package magicgoose.gomoku.ai
import LineInfo._
import java.util.Arrays

final object LineInfo {
  final val sz = 24
  final val OPEN = 0
  final val CLOSED = 1
  final val BROKEN = 2

  final val weights = Array(
      20, 2, 4, 1,
      200, 20, 40, 10,
      2000, 200, 400, 100,
      -20, -2, -4, -1,
      -200, -20, -40, -10,
      -2000, -200, -400, -100)
}
final class LineInfo() {
  final val patterns = Array.ofDim[Int](sz)
  var win = 0 //If someone wins, other data doesn't matter
  /* 2 players *
   **********************************************
   *   * open * closed * broken * closed broken *
   * 4 *      *        *        *               *
   * 3 *   4  *   ...  *        *               *
   * 2 *   0  *     1  *     2  *            3  *
   **********************************************
   */

  @inline final def apply(player: Int, ln: Int, offset: Int) = {
    patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + offset)
  }
  @inline final def apply(player: Int, ln: Int) = {
    patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4) +
      patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + 1) +
      patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + 2) +
      patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + 3)
  }

  final def +=(rhs: LineInfo) {
    if (rhs.win != 0) {
      win = rhs.win
    } else {
      var i = 0
      while (i < sz) {
        patterns(i) += rhs.patterns(i)
        i += 1
      }
    }
  }
  final def -=(rhs: LineInfo) {
    if (rhs.win != 0) {
      assert(rhs.win == win)
      win = 0
    }
    var i = 0
    while (i < sz) {
      patterns(i) = patterns(i) - rhs.patterns(i)
      i += 1
    }
  }
  final def reset() {
    Arrays.fill(patterns, 0)
    win = 0
  }

  override def toString = {
    (if (win == 0) {
      val l2 = patterns.length / 2
      def for_player(p: Int) = {
        val header = s"Player $p:\n"
        val records = (for (i <- p * l2 until (p + 1) * l2; if (patterns(i) != 0)) yield {
          s"${patterns(i)} ${
            (i % 4) match {
              case 0 => "open"
              case 1 => "closed"
              case 2 => "broken"
              case 3 => "closed broken"
            }
          } ${(i % (sz / 2)) / 4 + 2}s"
        })
        if (records.isEmpty) ""
        else header + records.mkString("\n") + "\n\n"
      }
      for_player(0) + for_player(1)
    } else {
      s"Player $win wins"
    })
  }
}
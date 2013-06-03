package magicgoose.gomoku.ai

import LineInfo._
import java.util.Arrays
final object LineInfo {
  final val targetLength = 5
  final val sz = 32
  
  final val OPEN = 0
  final val CLOSED = 1
  final val BROKEN = 2

  final val weights = Array(Array(
    20, 2, 4, 1,
    200, 20, 40, 10,
    2000, 200, 400, 100,
    100000, 100000, 100000, 100000,
    -40, -4, -8, -2,
    -400, -40, -80, -20,
    -4000, -400, -800, -200,
    -100000, -100000, -100000, -100000),
    Array(
    -40, -4, -8, -2,
    -400, -40, -80, -20,
    -4000, -400, -800, -200,
    -100000, -100000, -100000, -100000,
    20, 2, 4, 1,
    200, 20, 40, 10,
    2000, 200, 400, 100,
    100000, 100000, 100000, 100000))

  final val step = 100000

  final val WIN = Int.MaxValue / 2
  final val LOSS = (-WIN).toInt
  final val WIN1 = WIN / 2
  final val LOSS1 = LOSS / 2
  final val WIN2 = WIN1 / 2
  final val LOSS2 = LOSS1 / 2
}
final class LineInfo() {
  final val patterns = Array.ofDim[Int](sz)

  /* 2 players *
   **********************************************
   *   * open * closed * broken * closed broken *
   * 5 *      *        *        *               *   
   * 4 *      *        *        *               *
   * 3 *   4  *   ...  *        *               *
   * 2 *   0  *     1  *     2  *            3  *
   **********************************************
   */

  @inline final def apply(player: Int, ln: Int, offset: Int) = {
    patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + offset)
  }
  
  @inline final def inc(player: Int, ln: Int, offset: Int) = {
    patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + offset) += 1
  }
  
  @inline final def apply(player: Int, ln: Int) = {
    patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4) +
      patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + 1) +
      patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + 2) +
      patterns((1 - player) / 2 * LineInfo.sz / 2 + (ln - 2) * 4 + 3)
  }
  
  @inline final def win(player: Int) =
    this(player, 5, 0) + this(player, 5, 1) > 0

  final def +=(rhs: LineInfo) {
    var i = 0
    while (i < sz) {
      patterns(i) += rhs.patterns(i)
      i += 1
    }
  }

  final def -=(rhs: LineInfo) {
    var i = 0
    while (i < sz) {
      patterns(i) = patterns(i) - rhs.patterns(i)
      i += 1
    }
  }
  final def reset() {
    Arrays.fill(patterns, 0)
  }

  override def toString = {
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
  }
}
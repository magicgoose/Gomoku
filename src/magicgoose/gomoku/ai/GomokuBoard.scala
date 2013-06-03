package magicgoose.gomoku.ai

import java.util.Arrays
import scala.language.implicitConversions
import scala.util.Random
import scala.annotation.tailrec
/**
 * Class that encapsulates board state and AI-related variables and precalculated stuff
 */
class GomokuBoard private (
  private final val contents: Array[Int], // the board
  final val side_size: Int, // one dimensional size
  final val total_size: Int // number of cells
  ) {

  final val line_length = 5

  @volatile var pieces = 0

  val hashSize = 1 << 20
  class HashRecord {
    @volatile var hash: Long = Random.nextLong
    @volatile var depth: Int = -1
    @volatile var score: Int = 0
  }
  val hashTable =
    Array.fill(hashSize)(new HashRecord)

  val cellHashes =
    Array.fill(total_size)(Array.fill(2)(
      Random.nextLong()))
  def cellHash(index: Int) = cellHashes(index)(contents(index) + 1)

  @volatile private var hash = 0L
  @inline def getHash = hash

  def clamp(n: Long, range: Int): Int = {
    if (n >= 0) (n % range).toInt
    else {
      clamp(n + Long.MaxValue / 2, range)
    }
  }

  def hashLookup() = {
    hashTable(clamp(hash, hashSize))
  }

  // pre-calculated value, for every cell it contains all lines,
  // represented by array of its indices and instance of LineInfo
  private final val lines = { // position -> line number ->
    //(stats, line indexes)
    val lines_- = {
      array_tabulate(0, side_size, 1, y =>
        Array.range(side_size * y, side_size * (y + 1), 1))
    }
    val lines_| = {
      array_tabulate(0, side_size, 1, x =>
        Array.range(x, x + total_size, side_size))
    }
    val lines_/ = {
      array_tabulate(
        line_length - 1, 2 * side_size - line_length, 1, slice => {
          val bound = math.max(0, slice - side_size + 1)
          Array.range(
            slice + (side_size - 1) * bound,
            bound + side_size * (slice - bound + 1) - 1,
            side_size - 1)
        })
    }
    val lines_\ = {
      array_tabulate(
        line_length - 1, 2 * side_size - line_length, 1, slice => {
          val bound = math.max(0, slice - side_size + 1)
          Array.range(
            bound + side_size * (side_size - 1 - slice + bound),
            slice - bound + side_size * (side_size - bound) + 1,
            1 + side_size)
        })
    }
    val all_lines = Array.concat(lines_-, lines_|, lines_/, lines_\)
      .map(l => (new LineInfo(), l))
    Array.tabulate(total_size)(c => {
      (all_lines.filter(_._2.contains(c)))
    })
  }

  // summary of lines, used in evaluation function
  final val overall_line_info = new LineInfo()

  // reset state to new game
  final def reset_!() {
    lines.foreach(_.foreach(_._1.reset()))
    overall_line_info.reset()
    Arrays.fill(contents, 0.toByte)
    current_player = 1
    hash = 0
    pieces = 0
  }

  @volatile var current_player = 1
  final def update_!(coord: Int)(new_value: Int) = {
    val old_value = contents(coord)
    if (old_value != 0) hash ^= cellHashes(coord)((old_value + 1) / 2)
    if (!(new_value == 0 ||
      (old_value == 0 && new_value == current_player && winner() == 0))) {
      throw new Error(s"$coord, $old_value, $new_value\n" +
        s"${new_value == 0}, ${old_value == 0}," +
        s" ${new_value == current_player}, ${winner()}")
    }
    contents(coord) = new_value
    current_player *= -1
    val crossing_lines = lines(coord)
    // update line info
    array_foreach(crossing_lines)(t => {
      val (line_info, line_indexes) = t
      overall_line_info -= line_info
      line_info.reset()
      LineCounter.count_lines(line_indexes, contents, line_info)
      overall_line_info += line_info
    })
    // update hash
    if (new_value != 0) hash ^= cellHashes(coord)((new_value + 1) / 2)
    if (new_value == 0) pieces -= 1
    else pieces += 1
    assert(pieces >= 0)
  }

  @inline final def apply(coord: Int) = contents(coord)

  // for each cell contains array of indexes of "close enough" cells
  final val neighbours2 = Array.tabulate(total_size)(i => {
    val x = i % side_size
    val y = i / side_size
    (for (
      xx <- (0 max (x - 2)) to ((side_size - 1) min (x + 2));
      yy <- (0 max (y - 2)) to ((side_size - 1) min (y + 2));
      dx = math.abs(x - xx);
      dy = math.abs(y - yy);
      if (dx == 0 || dy == 0 || dx == dy)
    ) yield (xx + side_size * yy)).toArray
  })

  def winner() = {
    import LineInfo._
    val win1st = overall_line_info.win(1)
    val win2nd = overall_line_info.win(-1)

    assert(!(win1st && win2nd))

    if (win1st) 1
    else if (win2nd) -1
    else 0
  }

  def checkNeighbours(cell: Int): Boolean = {
    val neighbours = neighbours2(cell) // they are precomputed
    @tailrec def loop(ni: Int = 0): Boolean = {
      if (ni < neighbours.length) {
        (this(neighbours(ni)) != 0) || loop(ni + 1)
      } else false
    }
    loop(0)
  }

  /**
   * heuristic score for current player
   */
  def heur_score() = {
    import LineInfo._
    val win = winner()

    if (win != 0) (win * Int.MaxValue) // + plain_score
    else {
      val li = overall_line_info
      val threat_score =
        if (li(current_player, 4) >= 1)
          WIN1
        else if (li(-current_player, 4, OPEN) >= 1 
//            || li(-current_player, 4) >= 1
//            && 
//            li(-current_player, 3, OPEN) + 
//            li(-current_player, 3, BROKEN) >= 1
            )
          LOSS1
//        else if (li(current_player, 3, OPEN) +
//          li(current_player, 3, BROKEN) >= 1)
//          WIN2
        else if (li(-current_player, 4) + li(-current_player, 3, OPEN) + li(-current_player, 3, BROKEN) >= 2)
          LOSS2
        else 0
      if (threat_score != 0)
        threat_score
      else
        -dot(
          overall_line_info.patterns,
          LineInfo.weights((current_player + 1) / 2))
    }
  }

  @inline final def withMove(coord: Int)(fun: () => Boolean): Boolean = {
    update_!(coord)(current_player)
    val r = fun()
    update_!(coord)(0)
    r
  }

  override def toString() = {
    val cell_formatter = (x: Int) => {
      x match {
        case 1 => "X"
        case 0 => " "
        case -1 => "O"
      }
    }
    val hor_pad = "─" * side_size
    "┌" + hor_pad + "┐\n│" +
      contents.iterator.sliding(side_size, side_size).map(_.map(cell_formatter).mkString).mkString("│\n│") +
      "│\n└" + hor_pad + "┘\n"

  }
}

object GomokuBoard {
  def empty(side_size: Int) = {
    val total_size = side_size * side_size
    new GomokuBoard(Array.fill(total_size)(0), side_size, total_size)
  }
  def fromString(s: String) = {
    val stripped = s.split("\n").toSeq.tail.init.flatMap(x => x.toSeq.tail.init.map({
      case 'X' => 1
      case 'O' => -1
      case ' ' => 0
    }))
    val sz = perfectSquareRoot(stripped.length)
    if (sz <= 0) null
    else {
      val board = empty(sz)

      val pieces = stripped.zipWithIndex.filter(_._1 != 0)
      val pieces1 = pieces.iterator.filter(_._1 == 1).toList
      val pieces2 = pieces.iterator.filter(_._1 == -1).toList
      assert(Set(0, 1).contains(pieces1.length - pieces2.length))

      def fill(p1: List[(Int, Int)], p2: List[(Int, Int)]) {
        p1 match {
          case (t1, i1) :: p1s => {
            board.update_!(i1)(t1)
            p2 match {
              case (t2, i2) :: p2s => {
                board.update_!(i2)(t2)
                fill(p1s, p2s)
              }
              case _ =>
            }
          }
          case _ =>
        }
      }

      fill(pieces1, pieces2)

      board
    }

  }
}
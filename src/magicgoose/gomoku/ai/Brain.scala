package magicgoose.gomoku.ai

import scala.annotation.tailrec

trait GomokuBrain {
  def findPossibleMoves(): Indexed[Int]
  def findMove(player: Int): Int
}

class StupidGomokuBrain(val board: GomokuBoard) extends GomokuBrain {
  def findMove(player: Int): Int = {
    findPossibleMoves.maxBy(0)(move_rating(player, _))
  }

  private val tmpPossibleMoves = Array.fill(3)(GrowableArray.create[Int](board.total_size))
  def findPossibleMoves = findPossibleMoves(0)
  def findPossibleMoves(layer: Int): magicgoose.gomoku.ai.Indexed[Int] = {
    tmpPossibleMoves(layer).reset()

    var cell = 0
    while (cell < board.total_size) {
      val neighbours = board.neighbours2(cell)

      @tailrec def check(ni: Int): Boolean = {
        if (ni < neighbours.length) {
          (board(neighbours(ni)) != 0) || check(ni + 1)
        } else false
      }
      if (board(cell) == 0 && check(0)) {
        tmpPossibleMoves(layer).push(cell)
      }
      cell += 1
    }
    assert(tmpPossibleMoves(layer).length > 0)
    tmpPossibleMoves(layer)
  }

  final val WIN = Int.MaxValue
  final val LOSS = -(WIN - 1)
  final val WIN1 = WIN - 1
  final val LOSS1 = LOSS + 1
  final val WIN2 = WIN - 2

  def move_rating(player: Int, coord: Int, depth: Int = 0): Int = {
    board.update_!(coord)(player)
    val result = if (depth == 0) {
      line_stats_rating(board.overall_line_info, player)
    } else { // Something wrong here
      val possible_moves = findPossibleMoves(depth)
      -possible_moves.maxBy(0)(move_rating(-player, _, depth - 1))
    }
    board.update_!(coord)(0)
    result
  }
  def line_stats_rating(li: LineInfo, player: Int) = { // stats for position with `ls` and last moved player `player` for `player`
    import LineInfo._
    if (li.win == player)
      WIN
    else if (li.win == -player || li(-player, 4) >= 1)
      LOSS
    else if (li(player, 4, OPEN) >= 1 || li(player, 4) >= 2)
      WIN1
    else if (li(-player, 3, OPEN) >= 1 || li(-player, 3, BROKEN) >= 1)
      LOSS1
    else if (li(player, 4) + li(player, 3, OPEN) >= 2)
      WIN2
    else player * dot(li.patterns, LineInfo.weights)
  }
}
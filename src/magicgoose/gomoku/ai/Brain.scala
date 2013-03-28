package magicgoose.gomoku.ai

import scala.annotation.tailrec
/**
 * General interface for an implementation of AI-powered move searching
 */
trait GomokuBrain {
  /**
   * returns all canditates for next move
   */
  def findPossibleMoves(): Indexed[Int]

  /**
   * returns best move
   */
  def findMove(): Int
}

class SimpleGomokuBrain(val board: GomokuBoard) extends GomokuBrain {
  def findMove(): Int = {
    val move = nmab_m(4) // run negamax a/b search with depth 4
    assert(0 <= move && move < board.total_size && board(move) == 0) // check for correctness (for debugging)
    move
  }

  def findPossibleMoves() = {
    val possibleMoves = GrowableArray.create[Int](board.total_size)

    var cell = 0 // here we pick only moves that are at most 2 cells away from already used cell
    while (cell < board.total_size) {
      val neighbours = board.neighbours2(cell) // they are precomputed

      @tailrec def check(ni: Int): Boolean = {
        if (ni < neighbours.length) {
          (board(neighbours(ni)) != 0) || check(ni + 1)
        } else false
      }
      if (board(cell) == 0 && check(0)) {
        possibleMoves.push(cell)
      }
      cell += 1
    }
    assert(possibleMoves.length > 0)
    possibleMoves
  }
  /**
   * get heuristic score of the move
   */
  def test_move_heur(coord: Int) = {
    board.update_!(coord)(board.current_player) // make move
    val res = board.heur_score()
    board.update_!(coord)(0) // undo move
    res
  }
  
  /**
   * negamax a/b search, returns best move
   */
  def nmab_m(depth: Int, alpha: Int = -Int.MaxValue, b: Int = Int.MaxValue): Int = {
    val moves = findPossibleMoves().sortBy(test_move_heur(_)).take(24)
    var bestmove = 0
    var bestscore = alpha
    @inline def loop(i: Int): Int = {
      if (i < moves.length) {
        val recursedscore: Int = -nmab(moves(i), depth, -b, -bestscore)
        if (recursedscore > bestscore) {
          bestscore = recursedscore
          bestmove = moves(i)
        }
        if (bestscore >= b) {
          bestmove
        } else loop(i + 1)
      } else bestmove
    }
    loop(0)
  }
  
  /**
   * negamax a/b search, returns best score
   * this is "fail-soft" version - score may exceed the bound
   */
  def nmab(coord: Int, depth: Int, alpha: Int = -Int.MaxValue, b: Int = Int.MaxValue): Int = {
    board.update_!(coord)(board.current_player)
    val heur = board.heur_score()
    val result = if (math.abs(heur) >= LineInfo.WIN1 || depth == 0)
      heur
    else {
      val moves = findPossibleMoves().sortBy(test_move_heur(_)).take(24)
      var bestscore = alpha
      @inline def loop(i: Int): Int = {
        if (i < moves.length) {
          val recursedscore: Int = -nmab(moves(i), depth - 1, -b, -bestscore)
          if (recursedscore > bestscore) {
            bestscore = recursedscore
          }
          if (bestscore >= b) {
            bestscore
          } else loop(i + 1)
        } else bestscore
      }
      loop(0)
    }
    board.update_!(coord)(0)
    result
  }
}
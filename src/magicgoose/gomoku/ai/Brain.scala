package magicgoose.gomoku.ai

import scala.annotation.tailrec
import java.util.Arrays
/**
 * General interface for an implementation of AI-powered move searching
 */
//trait MoveSearcher {
//  /**
//   * returns all canditates for next move
//   */
//  def findPossibleMoves(): Indexed[Int]
//
//  /**
//   * returns best move
//   */
//  def findMove(): Int
//}

class MoveSearcher(val board: GomokuBoard) {
  val max_depth = 5

  def findMove(): Int = {
    if (board.pieces > 0) { // if not first move

      val move = search_move(max_depth)

      assert(0 <= move && move < board.total_size && board(move) == 0) // check for correctness (for debugging)
      move
    } else {
      board.total_size / 2
    }
  }

//  final val side = board.side_size
//  @inline final def distance(c1: Int, c2: Int): Int = {
//    val y1 = c1 / side
//    val x1 = c1 % side
//    val y2 = c2 / side
//    val x2 = c2 % side
//    math.abs(x2 - x1) + math.abs(y2 - y1)
//  }
  
  def findPossibleMoves(): GrowableArray[Int] = {
    import LineInfo._

    val player = board.current_player
    val li = board.overall_line_info

    if (li(player, 4) > 0) {
      var cell = 0
      while (cell < board.total_size) {
        if (board(cell) == 0 && board.withMove(cell)(() => li.win(player)))
          return GrowableArray.create1(cell)

        cell += 1
      }
    }

    val flt: Int => Boolean =
      if (li(-player, 4) > 0) {
        val open = li(-player, 4, OPEN)
        val total = li(-player, 4)

        board.withMove(_)(() => {
          li(-player, 4, OPEN) < open ||
            li(-player, 4) < total
        })
      } else {
        if (li(-player, 4) == 0 && (li(player, 3, OPEN) > 0 || li(player, 3, BROKEN) > 0)) {
          var cell = 0
          while (cell < board.total_size) {
            if (board(cell) == 0 && board.withMove(cell)(() => li(player, 4, OPEN) > 0))
              return GrowableArray.create1(cell)

            cell += 1
          }
        }

        val th3 = li(-player, 3, OPEN) + li(-player, 3, BROKEN)
        if (th3 > 0) {
          board.withMove(_)(() => {
            li(-player, 3, OPEN) + li(-player, 3, BROKEN) < th3
          })
        } else {
          x => true
        }
      }

    val possibleMoves = GrowableArray.create[Int](board.total_size - board.pieces)

    var cell = 0 // here we pick only moves that are at most 2 cells away from already used cell
    while (cell < board.total_size) {

      if (board(cell) == 0 && board.checkNeighbours(cell) && flt(cell)) {
        possibleMoves.push(cell)
      }
      cell += 1
    }
    assert(possibleMoves.length > 0)
    possibleMoves
  }

  /**
   * simple score, with hash lookup
   */
  def getSimpleScore() = {
//    val he = board.hashLookup()
//    if (he.hash == board.getHash && he.depth >= 0) {
//      he.score
//    } else
      board.heur_score()
  }

  /**
   * get heuristic score of the move
   */
  def test_move_heur(coord: Int) = {
    board.update_!(coord)(board.current_player) // make move
    val res =
      getSimpleScore()
    board.update_!(coord)(0) // undo move
    res
  }

  /**
   * returns best move
   */
  def search_move(depth: Int, alpha: Int = -Int.MaxValue, b: Int = Int.MaxValue): Int = {
    val moves = findPossibleMoves()
    if (moves.length == 1) return moves.head
    if (moves.length == 0) throw new Error("This should not happen! No available moves.")
    moves.sortByInplace(test_move_heur)
    if (board.pieces == 1) moves.trim(2)
    if (moves.head <= -LineInfo.WIN1) moves.head
    else {
      var bestmove = 0
      var bestscore = alpha
      @inline def loop(i: Int): Int = {
        if (i < moves.length) {
          val recursedscore: Int = -search_score(moves(i), depth, -b, -bestscore)
          if (recursedscore > bestscore) {
            bestscore = recursedscore
            bestmove = moves(i)
          }
          if (bestscore >= LineInfo.WIN1) {
            bestmove
          } else loop(i + 1)
        } else bestmove
      }
      loop(0)
    }
  }

  /**
   * negamax a/b search, returns best score
   */
  def search_score(coord: Int, depth: Int, alpha: Int, b: Int): Int = {
//    def threats4(p: Int) =
//      board.overall_line_info(p, 4)

    //    def threats3(p: Int) =
    //      board.overall_line_info(p, 3, LineInfo.OPEN) +
    //        board.overall_line_info(p, 3, LineInfo.BROKEN)

//    val player = board.current_player
//    val threats4_before = threats4(player)
    //    val threats3_before = threats3(player)
    assert(board(coord) == 0)

    board.update_!(coord)(board.current_player)

//    val he = board.hashLookup()
    val result =
//      if (he.hash == board.getHash && he.depth == depth) {
//        //        println("score found in table")
//        //        if (!board.equalsArray(he.board))
//        //          println("collision")
//        he.score
//      } else
      {
        val heur = board.heur_score()
        if (math.abs(heur) >= LineInfo.WIN1 || depth <= 0)
          heur
        else {
          val next_depth =
//            if (threats4(player) > threats4_before)
//              depth
//            else
              depth - 1

          val moves = findPossibleMoves().sortByInplace(test_move_heur) //.trim(4)
          if (moves.length == 0) {
            board.update_!(coord)(0)
            //println("no moves:\n" + board)
            return Int.MaxValue //-
          }
          var bestscore = alpha
          @inline def loop(i: Int): Int = {
            if (i < moves.length) {
              val recursedscore: Int = -search_score(moves(i), next_depth, -b, -bestscore)
              if (recursedscore > bestscore) {
                bestscore = recursedscore
              }
              if (bestscore >= b) {
                bestscore
                //b
              } else loop(i + 1)
            } else bestscore
          }
          val r = loop(0)
//          he.hash = board.getHash
//          he.depth = depth
//          he.score = r
          //          board.copyTo(he.board)
          r
        }
      }

    board.update_!(coord)(0)
    result
  }
}
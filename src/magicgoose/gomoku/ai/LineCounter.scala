package magicgoose.gomoku.ai

object LineCounter {

  def count_lines(row_ref: Array[Int], data: Array[Int], addResult: LineInfo): Unit = {
    import LineInfo._
    
    assert(row_ref.length >= targetLength)
    def count_for_player(player: Int): Unit = {
      def count_in_window(begin: Int, end: Int): Unit = {
        if (end - begin >= targetLength) {
          var count_l = 0
          var count_r = 0
          var space_before = false
          var last_space = false
          var last_broken = false
          var broken = false

          @inline def proc(t: Int, n: Int) = {
            t match {
              case `player` => mark(n)
              case 0 => space(n)
            }
          }

          def mark(n: Int) = {
            if (broken) {
              count_r = n
            } else {
              count_l = n
            }
            last_space = false
          }

          def space(n: Int) = {
            if (count_l == 0) {
              space_before = true
            } else {
              if (broken) {
                add(count_l + count_r, true, space_before)
                space_before = true
                last_broken = true
                if (n == 1) {
                  count_l = count_r
                } else {
                  broken = false
                  count_l = 0
                }
              } else { // broken == false
                last_broken = false
                if (n == 1) {
                  broken = true
                } else { // more than 1 spaces and there is single line segment on the left
                  add(count_l, false, space_before)

                  space_before = true
                  count_l = 0
                }
              }
            }
            count_r = 0
            last_space = true
          }

          def finish() = {
            if (count_l > 0 && !last_broken) {
              add(count_l + count_r, broken && (count_r > 0), space_before && last_space)
            }
          }

          def add(count: Int, broken: Boolean, open: Boolean) = {
            if (count >= 2) {
              var offset = 0
              if (broken) offset += BROKEN
              if (!open) offset += CLOSED
              addResult.inc(player, math.min(targetLength, count), offset)
            }
          }

          var i = begin + 1
          var v = data(row_ref(begin))
          var count = 1
          while (i < end) {
            val t = data(row_ref(i)) 
            if (t != v) {
              proc(v, count)
              v = t
              count = 1
            } else {
              count += 1
            }
            i += 1
          }
          proc(v, count)
          finish()
        }
      }

      var begin = 0
      var i = 0
      while (i < row_ref.length) {
        if (-player == data(row_ref(i))) {
          count_in_window(begin, i)
          begin = i + 1
        }
        i += 1
      }
      count_in_window(begin, row_ref.length)
    }

    count_for_player(1)
    count_for_player(-1)
  }
}
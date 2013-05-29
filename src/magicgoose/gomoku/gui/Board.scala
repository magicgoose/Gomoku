package magicgoose.gomoku.gui

import javax.swing.JPanel
import java.awt.GridLayout
import javax.swing.BorderFactory
import java.awt.Color
import java.awt.event.MouseListener
//import magicgoose.gomoku.ai.perfectSquareRoot

class Board(val content: Array[Cell], val handler: (Int, Cell) => Unit) extends JPanel {
  val side_length = perfectSquareRoot(content.length)
  assert(side_length > 0)
  val grid = new GridLayout(side_length, side_length, -1, -1)

  setLayout(grid)
  foreach_i(content)((index, cell) => {
    cell.setBorder(BorderFactory.createLineBorder(Color.black, 1))

    add(cell)
    cell.addMouseListener(new MouseListener {
      def mouseClicked(e: java.awt.event.MouseEvent) {}
      def mouseEntered(e: java.awt.event.MouseEvent) {}
      def mouseExited(e: java.awt.event.MouseEvent) {}
      def mousePressed(e: java.awt.event.MouseEvent) {
        handler(index, cell)
      }
      def mouseReleased(e: java.awt.event.MouseEvent) {}
    })
  })

  def perfectSquareRoot(n: Int) = {
    val h = n & 0xF
    var t = 0
    if (!(h > 9) && (h != 2 && h != 3 && h != 5 && h != 6 && h != 7 && h != 8) && {
      t = math.floor(Math.sqrt(n.toDouble) + 0.5).toInt
      t * t == n
    }) t
    else -1
  }
}
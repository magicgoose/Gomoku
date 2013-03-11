package magicgoose.gomoku.gui

import javax.swing.JPanel
import java.awt.GridLayout
import javax.swing.BorderFactory
import java.awt.Color
import java.awt.event.MouseListener

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
}
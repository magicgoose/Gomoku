package magicgoose.gomoku.gui

import javax.swing.JPanel
import java.awt.Color
import java.awt.Graphics
import javax.swing.JLabel
import java.awt.Graphics2D
import java.awt.RenderingHints
import javax.swing.SwingConstants
import java.lang.ref.WeakReference
import Cell._
import java.awt.Dimension

object Cell {
  private var show_index = true
  private var cells = List.empty[WeakReference[Cell]]
  def setIndexesVisible(x: Boolean) = {
    show_index = x
    cells = cells.filter(ref => {
      val p = ref.get()
      val res = (p != null)
      if (res) {
        p.setIndexVisible(show_index)
      }
      res
    })
  }
  final val fig_padding = 2
}
class Cell() extends JLabel {
  setOpaque(false)
  setHorizontalAlignment(SwingConstants.CENTER)
  cells = new WeakReference(this) :: cells
  private var _index = 0
  private var cell_value_ = 0

  def setIndexVisible(x: Boolean) {
    setText(if (x && cell_value_ != 0) _index.toString else "")
  }
  def cell_value = cell_value_
  def setCellValue(x: Int, index: Int) {
    cell_value_ = x
    _index = index
    setIndexVisible(Cell.show_index)
    repaint()
  }
  private var font_size = 0f
  override def paint(g: Graphics) = {
    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val maxrad = Math.min(getSize().width, getSize().height)
    val new_font_size = maxrad.toFloat / 2
    if (new_font_size != font_size) {
      font_size = new_font_size
      setFont(getFont.deriveFont(font_size))
    }
    super.paint(g)
    if (cell_value != 0) {
      val x = (getSize().width - maxrad) / 2 + fig_padding
      val y = (getSize().height - maxrad) / 2 + fig_padding
      val xl = maxrad - fig_padding * 2 - 1
      val yl = maxrad - fig_padding * 2 - 1
      
      if (cell_value == -1) {
        g.setColor(Color.blue)
        g.drawOval(x, y, xl, yl)
      } else {
        g.setColor(Color.red)
        g.drawLine(x, y, x + xl, y + yl)
        g.drawLine(x, y + yl, x + xl, y)
      }
    }
  }

}
package magicgoose.gomoku.gui

import java.awt.{ BorderLayout, EventQueue, GraphicsEnvironment }
import javax.swing.{ JFrame, JLabel, UIManager }
import magicgoose.gomoku.ai.GomokuBoard
import magicgoose.gomoku.gui.builder.GuiBuilder
import GomokuMainWindow._
import javax.swing.JTextArea
import magicgoose.gomoku.ai.LineInfo
import javax.swing.JOptionPane
import magicgoose.gomoku.ai.SimpleGomokuBrain
object GomokuMainWindow {

  val size = 15

  def main(args: Array[String]) {
    EventQueue.invokeLater(new Runnable {
      def run() {
        try {
          UIManager.setLookAndFeel(
            UIManager.getSystemLookAndFeelClassName());
        } catch { case _: Throwable => }

        val window = new GomokuMainWindow
        window.frmMain.setVisible(true)
      }
    })
  }
}
class GomokuMainWindow {
  private var frmMain: JFrame = _
  initialize()

  private def initialize() {

    import GuiBuilder._

    def warn(title: String, msg: String) {
      JOptionPane.showMessageDialog(frmMain, msg, title, JOptionPane.WARNING_MESSAGE)
    }

    frmMain = new JFrame()
    frmMain.setTitle("Gomoku GUI")
    val display_mode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode()
    val (x, y) = (display_mode.getWidth(), display_mode.getHeight())
    frmMain.setBounds(x / 9, y / 9, 7 * x / 9, 7 * y / 9)
    frmMain.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val content = frmMain.getContentPane()
    content.setLayout(new BorderLayout(8, 8))
    val time_label = new JLabel
    val info_label = new JTextArea
    info_label.setEditable(false)
    info_label.setFont(info_label.getFont().deriveFont(14f))
    val cells = Array.fill(size * size)(new Cell())
    val ai_board = GomokuBoard.empty(size)
    var move_number = 1
    var player = 1
    var moves = List.empty[Int] // undo history
    var move_ui_updater = Option.empty[Int => Unit] // i wish i had a forward reference

    def makeMove(index: Int) {
      ai_board.update_!(index)(player)
      move_ui_updater.foreach(_(index))
      move_number += 1
      player = -player
      moves = index :: moves
    }

    val bot = new SimpleGomokuBrain(ai_board)

    def checkWin() = {
      val sc = ai_board.heur_score
      if (math.abs(sc) == Int.MaxValue)
        math.signum(sc)
      else 0
    }

    val board = new Board(cells, (index, cell) => {
      //try {
        if (checkWin == 0)
          makeMove(index)
        if (checkWin == 0) {
          val t1 = System.currentTimeMillis()
          makeMove(bot.findMove(player))
          time_label.setText(s"Time spend thinking: ${System.currentTimeMillis() - t1}ms")
        }
//      } catch {
//        case e: Throwable => {
//          warn("Illegal move", format_exception(e))
//        }
//      }
    })
    move_ui_updater = Some(i => {
      board.content(i).setCellValue(player, move_number)
      info_label.setText(ai_board.overall_line_info.toString)
    })

    def reset_game() {
      ai_board.reset_!()
      cells.foreach(_.setCellValue(0, 0))
      player = 1
      move_number = 1
      info_label.setText("")
    }

    def undo_move_pair() {
      moves match {
        case m1 :: m2 :: ms => {
          moves = ms
          move_number -= 2
          board.content(m1).setCellValue(0, 0)
          board.content(m2).setCellValue(0, 0)
          ai_board.update_!(m1)(0)
          ai_board.update_!(m2)(0)
          info_label.setText(ai_board.overall_line_info.toString)
        }
        case _ => {
          warn("Error", "There are no moves to undo!")
        }
      }
    }

    val board_container = new AspectRatioContainer(1.0, board)

    content.add(board_container, BorderLayout.CENTER)
    content.add(panel_box(
      button("Undo move", undo_move_pair),
      info_label, time_label), BorderLayout.LINE_END)

    val menubar = menuBar(
      menu("Game",
        menuItem("Start new game",
          action = reset_game)),
      menu("Settings",
        //menuItem("Engine settings..."),
        menuItemCheck("Show move numbers",
          checked = true,
          accel = "VK_F2",
          action = Cell.setIndexesVisible)) /*,
      menu("Help",
        menuItem("Show help",
          accel = "VK_F1"),
        menuItem("About Gomoku",
          accel = "VK_F12"))*/ )
    frmMain.setJMenuBar(menubar)
  }

}
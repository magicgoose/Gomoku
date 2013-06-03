package magicgoose.gomoku.gui

import java.awt.{ BorderLayout, EventQueue, GraphicsEnvironment }
import javax.swing.{ JFrame, JLabel, UIManager }
import magicgoose.gomoku.ai.GomokuBoard
import magicgoose.gomoku.gui.builder.GuiBuilder
import GomokuMainWindow._
import javax.swing.JTextArea
import javax.swing.JOptionPane
import magicgoose.gomoku.ai.MoveSearcher
import java.io.FilenameFilter
import java.io.File
import javax.swing.JFileChooser
import java.awt.Component
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

  implicit def fun2runnable(fun: () => Unit) =
    new Runnable { def run() { fun() } }

  private def runAsync[R](task: () => R)(onFinish: R => Unit) {
    val branch = new Thread(() => {
      val taskResult = task()
      EventQueue.invokeLater(() => { onFinish(taskResult) })
    })
    branch.setDaemon(true)
    branch.start()
  }

  private def initialize() {

    import GuiBuilder._

    def warn(title: String, msg: String) {
      JOptionPane.showMessageDialog(frmMain, msg, title, JOptionPane.WARNING_MESSAGE)
    }

    @volatile var busy = false
    val title = "Gomoku"
    val busy_title = title + " (thinking...)"
    def update_title() {
      frmMain.setTitle(if (busy) busy_title else title)
    }
    frmMain = new JFrame()
    update_title()

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
    info_label.setVisible(false)
    val cells = Array.fill(size * size)(new Cell())
    var ai_board = GomokuBoard.empty(size)
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

    val searcher = new MoveSearcher(ai_board)

    def winner = {
      ai_board.winner()
    }

    def checkWin() {
      val c = winner
      if (c != 0) {
        warn("Game over", s"Player ${
          if (c > 0) "1 (cross)"
          else "2 (circle)"
        } wins!")
      }
    }

    def onCellClick(index: Int, cell: Cell) = {
      if (!busy) {
        if (winner != 0) {
          warn("Warning", "Game is finished. Undo some moves or start a new game.")
        } else if (ai_board(index) != 0) {
          warn("Warning", "FYI: Gomoku rules allow placing marks only in empty cells.")
        } else {
          if (winner == 0)
            makeMove(index)
          if (winner == 0) {
            computer_move()
          } else {
            checkWin()
          }
        }
      }
    }

    def switch_side() {
      if (!busy && winner == 0) {
        computer_move()
      }
    }

    def sync() {
      if (ai_board.side_size != 15) throw new Error
      move_number = 1
      var i = 0
      while (i < 15 * 15) {
        if (ai_board(i) != 0) {
          val piece = ai_board(i)
          player = piece
          move_ui_updater.foreach(_(i))
          move_number += 1
        }

        i += 1
      }
      player = ai_board.current_player
    }

    def load_position() {
      val ololo = new JFileChooser
      ololo.setFileFilter(((file: File) => {
        file.getName().endsWith(".txt") ||
          file.isDirectory()
      }, "Position text files"))
      ololo.showOpenDialog(frmMain) match {
        case JFileChooser.APPROVE_OPTION => {
          val fn = ololo.getSelectedFile().getAbsolutePath()
          val text = slurp(fn)
          ai_board = GomokuBoard.fromString(text)
          sync()
        }
      }
    }

    def computer_move() {
      assert(!busy)
      busy = true
      update_title()

      runAsync(() => {
        val t1 = System.currentTimeMillis()
        (searcher.findMove(), System.currentTimeMillis() - t1)
      })(result => {
        val (move, time) = result
        makeMove(move)
        time_label.setText(s"Time spend thinking: ${time}ms")
        busy = false
        update_title()
        checkWin()
      })
    }

    val board = new Board(cells, onCellClick)
    move_ui_updater = Some(i => {
      board.content(i).setCellValue(player, move_number)
      updateInfo()
    })

    def updateInfo() {
      info_label.setText(ai_board.overall_line_info.toString + //"\nHash: " + ai_board.getHash + "\n" +
        "\nCurrent player: " + ai_board.current_player)
    }

    def toggleVisible(c: Component) = {
      c.setVisible(_)
    }

    def reset_game() {
      if (!busy) {
        ai_board.reset_!()
        cells.foreach(_.setCellValue(0, 0))
        player = 1
        move_number = 1
        info_label.setText("")
      }
    }

    def undo_move() {
      if (!busy) {
        moves match {
          case m :: ms => {
            moves = ms
            move_number -= 1
            player *= -1
            board.content(m).setCellValue(0, 0)
            ai_board.update_!(m)(0)
            updateInfo()
          }
          case _ => {
            warn("Error", "There are no moves to undo!")
          }
        }
      }
    }

    def change_strength(x: Int) {
      if (!busy) {
        searcher.max_depth = x
      }
    }

    val board_container = new AspectRatioContainer(1.0, board)

    content.add(board_container, BorderLayout.CENTER)
    content.add(panel_box(
      intSlider(name = "AI strength", value = 4, min = 1, max = 6, handler = change_strength),
      button("Undo move", undo_move),
      button("Force computer move & Switch side", switch_side),
      button("Load position", load_position),
      info_label, time_label)(), BorderLayout.LINE_END)

    val menubar = menuBar(
      menu("Game",
        menuItem("Start new game",
          action = reset_game)),
      menu("Settings",
        //menuItem("Engine settings..."),
        menuItemCheck("Show move numbers",
          checked = true,
          accel = "VK_F2",
          action = Cell.setIndexesVisible),
        menuItemCheck("Show debug info",
          checked = false,
          accel = "VK_F3",
          action = toggleVisible(info_label))) /*,
      menu("Help",
        menuItem("Show help",
          accel = "VK_F1"),
        menuItem("About Gomoku",
          accel = "VK_F12"))*/ )
    frmMain.setJMenuBar(menubar)
  }

}
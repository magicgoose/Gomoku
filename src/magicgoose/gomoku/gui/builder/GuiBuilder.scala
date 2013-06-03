package magicgoose.gomoku.gui.builder

import javax.swing.JMenu
import javax.swing.JMenuBar
import javax.swing.JMenuItem
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JCheckBoxMenuItem
import java.awt.event.ItemListener
import java.awt.event.ItemEvent
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JPanel
import javax.swing.BoxLayout
import javax.swing.JSlider
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import javax.swing.JLabel

object GuiBuilder {
  def menuBar(items: JMenu*): JMenuBar = {
    val r = new JMenuBar
    items.foreach(r.add(_))
    r
  }
  def menu(name: String, items: JMenuItem*): JMenu = {
    val r = new JMenu(name)
    items.foreach(r.add(_))
    r
  }
  private def setAccel(target: JMenuItem, accel: String) {
    if (!accel.isEmpty()) {
      try {
        target.setAccelerator(KeyStroke.getKeyStroke(classOf[KeyEvent].getField(accel).getInt(null), 0))
      } catch {
        case _: Throwable => println("Unknown key code: " + accel)
      }
    }
  }
  def menuItem(name: String, accel: String = "", action: () => Unit = () => {}): JMenuItem = {
    val r = new JMenuItem(name)
    r.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        action()
      }
    })
    setAccel(r, accel)
    r
  }
  def menuItemCheck(name: String, accel: String = "", checked: Boolean = false, action: Boolean => Unit = x => {}): JCheckBoxMenuItem = {
    val r = new JCheckBoxMenuItem(name)
    r.setSelected(checked)
    r.addItemListener(new ItemListener {
      def itemStateChanged(e: java.awt.event.ItemEvent) {
        val checked = e.getStateChange() == ItemEvent.SELECTED
        action(checked)
      }
    })
    setAccel(r, accel)
    r
  }

  def intSlider(name: String, value: Int, min: Int, max: Int, handler: Int => Unit) = {
    def caption(x:Int)=name+": "+x+" "
    val l = new JLabel(caption(value))
    val s = new JSlider
    s.setMaximum(max)
    s.setMinimum(min)
    s.setValue(value)
    s.setMajorTickSpacing(1)
    s.setSnapToTicks(true)
    s.setPaintTicks(true)
    s.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent) {
        val v = s.getValue()
        handler(v)
        l.setText(caption(v))
      }
    })
    panel_box(l, s)(true)
  }

  def button(name: String, action: () => Unit = () => {}) = {
    val b = new JButton
    b.setText(name)
    b.addActionListener(new ActionListener {
      def actionPerformed(e: java.awt.event.ActionEvent) {
        action()
      }
    })
    b
  }
  def panel_box(contents: JComponent*)(horizontal: Boolean = false) = {
    val panel = new JPanel
    panel.setLayout(new BoxLayout(panel, if (horizontal) BoxLayout.LINE_AXIS else BoxLayout.PAGE_AXIS))
    contents.foreach(panel.add(_))
    panel
  }
  //  def frame(title: String) = {
  //
  //  }
}
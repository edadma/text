//@
package xyz.hyperreal.text

import scala.swing.{SimpleSwingApplication, MainFrame}


object Main extends SimpleSwingApplication {

  def top =
    new MainFrame {
      title = "Simple Text Editor"
      contents = new TextPanel
      visible = true
    }

}

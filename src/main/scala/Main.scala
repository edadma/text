//@
package xyz.hyperreal.text

import java.awt.Font

import scala.swing.{MainFrame, SimpleSwingApplication}


object Main extends SimpleSwingApplication {

  val textfont = new Font( "Monospaced", Font.PLAIN, 14 )

  def top =
    new MainFrame {
      val buffer = new TextBuffer( textfont, peer.getFontMetrics(textfont).getFontRenderContext )

      title = "Simple Text Editor"
      contents = new TextPanel( 25, 80, buffer )
    }

}

//@
package xyz.hyperreal.text

import java.awt.font.FontRenderContext
import java.awt.Font

import scala.swing.{MainFrame, SimpleSwingApplication}


object Main extends SimpleSwingApplication {

  val textfont = new Font( "Monospaced", Font.PLAIN, 20 )
//  val textfont = new Font( "DejaVu Sans Mono", Font.PLAIN, 20 )
//  val textfont = new Font( "FreeMono", Font.PLAIN, 20 )

  def top =
    new MainFrame {
      val buffer = new TextBuffer( textfont, new FontRenderContext(null, true, true) )//todo: frc needs to be settable after TextBuffer has been instantiated .getFontMetrics(textfont).getFontRenderContext )
			var p = Pos( 0, 0 )

      p = buffer.insert( "abcd", p )
      p = buffer.insert( "zxcv", p )
//      buffer.insert( "a\tas\td\tf\t\t|\n123456789012345678901\n         111111111122", p )
//      for (i <- 1 to 25) {
//        p = buffer.insertEach( "a"*80, p )
//
//        if (i < 25)
//          p = buffer.newline( p )
//      }

      title = "Simple Text Editor"
      contents = new TextPanel( 80, 25, buffer )
    }

}

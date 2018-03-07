//@
package xyz.hyperreal.text

import java.awt.font.FontRenderContext
import java.awt.{Font, GraphicsEnvironment}

import scala.swing.{MainFrame, SimpleSwingApplication}


object Main extends SimpleSwingApplication {

//  println( GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toList filter (_ containsSlice "Nimbus") )

  val textfont = new Font( "Monospaced", Font.PLAIN, 16 )

  def top =
    new MainFrame {
      val buffer = new TextBuffer( textfont, new FontRenderContext(null, true, true) )//todo: frc needs to be settable after TextBuffer has been instantiated .getFontMetrics(textfont).getFontRenderContext )
			var p = Pos( 0, 0 )

	    p = buffer.insertEach( "asdfASDFasdfASDF", p )
	    p = buffer.newline( p )
      buffer.insert( "asdfASDFasdfASDF\n[asdf][][]{}{}{}\nTTTTTTTTTTTTTT", p )
      title = "Simple Text Editor"
      contents = new TextPanel( 25, 80, buffer )
    }

}

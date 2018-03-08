package xyz.hyperreal.text

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform


object Main extends App {

  val buf = new TextBuffer( new Font("Monospaced", Font.PLAIN, 14), new FontRenderContext(new AffineTransform(), true, true) )

  buf.show
  buf.glyphs( 'a', 0, 0 )
  buf.glyphs( 'b', 0, 1 )
  buf.show

}
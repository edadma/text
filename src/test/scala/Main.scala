package xyz.hyperreal.text

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform


object Main extends App {

  val buf = new TextBuffer( new Font("Monospaced", Font.PLAIN, 14), new FontRenderContext(new AffineTransform(), true, true) )

  buf.show

}
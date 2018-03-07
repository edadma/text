//@
package xyz.hyperreal.text

import java.awt.Color._
import java.awt.RenderingHints

import scala.swing.{Graphics2D, Panel}
import scala.swing.Swing._
import scala.swing.event.{Key, KeyPressed, KeyTyped}


class TextPanel( rows: Int, cols: Int, buffer: TextBuffer ) extends Panel {

  val (width, height) = {
    val x = buffer.font.createGlyphVector( buffer.frc, "X" )

    (x.getLogicalBounds.getWidth, x.getLogicalBounds.getHeight)
  }

  var curpos = Pos( 0, 0 )

  buffer addView this

  preferredSize = ((width*cols).toInt, (height*rows).toInt)
  background = BLACK
  foreground = LIGHT_GRAY

  listenTo( keys )

  reactions += {
    case KeyTyped(_, c, _, _) =>
      curpos = buffer.insertGlyphs( c.toString, curpos )
  }

  focusable = true
  requestFocus

  override def paintComponent( g: Graphics2D ): Unit = {

    super.paintComponent( g )
    g.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )

    val extract = buffer.extract( 0, buffer.rows min 25 )

    var rowCount = 0

    for (l <- extract.rows) {
      var colCount = 0

      for (r <- l) {
        g.drawGlyphVector( r, (width*colCount).toFloat, (height*rowCount + height).toFloat )
        colCount += r.getNumGlyphs
      }

      rowCount += 1
    }

  }

}
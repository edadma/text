//@
package xyz.hyperreal.text

import java.awt.Color

import scala.swing.{Graphics2D, Panel}
import scala.swing.Swing._
import scala.swing.event.{Key, KeyPressed, KeyTyped}


class TextPanel( rows: Int, cols: Int, buffer: TextBuffer ) extends Panel {

//  println( buffer.font.createGlyphVector( buffer.frc, Array.fill(cols)('j') ).getVisualBounds )

  val maxCharBounds = buffer.font.getMaxCharBounds( buffer.frc )

  var row = 0
  var col = 0

  buffer addView this

  preferredSize = ((maxCharBounds.getWidth*cols).toInt, (maxCharBounds.getHeight*rows).toInt)

  listenTo( keys )

  reactions += {
    case KeyTyped(_, c, _, _) =>
      buffer.insert( c, row, col )

      if (c == '\n') {
        row += 1
        col = 0
      } else
        col += 1
  }

  focusable = true
  requestFocus

  override def paintComponent( g: Graphics2D ): Unit = {

    val extract = buffer.extract( 0, buffer.rows min 25 )

    var rowCount = 0

    for (l <- extract.rows) {
      var colCount = 0

      for (r <- l) {
        val bounds = r.getVisualBounds

        g.setColor( Color.BLACK )
        g.drawGlyphVector( r, (maxCharBounds.getWidth*colCount).toFloat, (maxCharBounds.getHeight*rowCount + maxCharBounds.getHeight + bounds.getY).toFloat )
        colCount += r.getNumGlyphs
      }

      rowCount += 1
    }

  }

}
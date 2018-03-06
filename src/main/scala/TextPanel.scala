//@
package xyz.hyperreal.text

import scala.swing.{Graphics2D, Panel}
import scala.swing.Swing._


class TextPanel( rows: Int, cols: Int, buffer: TextBuffer ) extends Panel {

  println( buffer.font.createGlyphVector( buffer.frc, Array.fill(cols)('j') ).getVisualBounds )

  val maxCharBounds = buffer.font.getMaxCharBounds( buffer.frc )

  preferredSize = ((maxCharBounds.getWidth*cols).toInt, (maxCharBounds.getHeight*rows).toInt)

  override def paintComponent( g: Graphics2D ): Unit = {

    val extract = buffer.extract( 0, 25 )

    for (l <- extract.rows; r <- l) {
      g.drawGlyphVector( )
    }

  }

}
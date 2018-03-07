//@
package xyz.hyperreal.text

import java.awt.Color._
import java.awt.{BasicStroke, RenderingHints}
import java.awt.geom.{Line2D, Rectangle2D}

import javax.swing.Timer

import scala.swing.{Graphics2D, Panel}
import scala.swing.Swing._
import scala.swing.event.{Key, KeyPressed, KeyTyped}


class TextPanel( rows: Int, cols: Int, buffer: TextBuffer ) extends Panel {

  val (width, height, ascent) = {
    val bounds = buffer.font.createGlyphVector( buffer.frc, "X" ).getLogicalBounds

    (bounds.getWidth, bounds.getHeight, -bounds.getY)
    }
  var showcursor = true
  val blink = new Timer( 500, ActionListener(_ => {showcursor = !showcursor; repaint}) )//todo: restart timer and turn on curson whenever buffer is mutated
  var curpos = Pos( 0, 0 )
  var selection: Option[Pos] = None

  buffer addView this
  blink.start

  preferredSize = ((width*cols).toInt, (height*rows).toInt)
  background = BLACK
  foreground = LIGHT_GRAY
  peer.setFocusTraversalKeysEnabled( false )
  listenTo( keys )

  reactions += {
    case KeyTyped( _, '\n', _, _ ) => curpos = buffer.newline( curpos )
    case KeyTyped( _, '\b', _, _ ) => buffer.backspace( curpos ) foreach (curpos = _)
    case KeyTyped( _, '\t', _, _ ) => curpos = buffer.tab( curpos )
    case KeyTyped( _, c, _, _ ) => curpos = buffer.insertGlyphs( c.toString, curpos )
    case KeyPressed( _, Key.Right, m, _ ) =>
      if (m == Key.Modifier.Shift) {
        if (selection isEmpty)
          selection = Some( curpos )
      }

      buffer.right( curpos ) foreach (curpos = _)

     repaint
    case KeyPressed( _, Key.Left, _, _ ) =>
      buffer.left( curpos ) foreach (curpos = _)
      repaint
    case KeyPressed( _, Key.Up, _, _ ) =>
      buffer.up( curpos ) foreach (curpos = _)
      repaint
    case KeyPressed( _, Key.Down, _, _ ) =>
      buffer.down( curpos ) foreach (curpos = _)
      repaint
    case KeyPressed( _, Key.Delete, _, _ ) =>
      buffer.delete( curpos )
   }

  focusable = true
  requestFocus

  override def paintComponent( g: Graphics2D ): Unit = {

    super.paintComponent( g )
    g.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )

    val extract = buffer.extract( 0, buffer.rows min 25 )

    selection foreach { s =>
      def box( row: Int, from: Int, to: Int ): Unit = {
        val y = row*height

        g fill new Rectangle2D.Double( from*width, y, (to - from + 1)*width, height )
      }

      val (from, to) = if (curpos <= s) (curpos, s) else (s, curpos)
      val c = g.getColor

      g setColor BLUE

      if (from.row == to.row)
        box( from.row, from.col, to.col )
      else {
        box( from.row, from.col, extract.rows(from.row)._1 - 1 )
      }

      g setColor c
    }

    var rowCount = 0

    for ((_, l) <- extract.rows) {
      var colCount = 0

      for (r <- l) {
        g.drawGlyphVector( r, (width*colCount).toFloat, (height*rowCount + ascent).toFloat )
        colCount += r.getNumGlyphs
      }

      rowCount += 1
    }

    val x = curpos.col*width
    val y = curpos.row*height + ascent + 2

    if (showcursor) {
      g.setStroke( new BasicStroke(3) )
      g.draw( new Line2D.Double(x, y, x + width, y) )
    }
  }

}
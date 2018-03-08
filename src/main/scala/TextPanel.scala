//@
package xyz.hyperreal.text

import java.awt.Color._
import java.awt.{BasicStroke, RenderingHints}
import java.awt.geom.{Line2D, Rectangle2D}

import javax.swing.Timer

import scala.swing.{Graphics2D, Panel}
import scala.swing.Swing._
import scala.swing.event.{Key, KeyPressed, KeyTyped}


class TextPanel( cols: Int, rows: Int, buffer: TextBuffer ) extends Panel {

  val drawboxes = true
  val (width, height, ascent) = {
    val bounds = buffer.font.createGlyphVector( buffer.frc, "X" ).getLogicalBounds

    (bounds.getWidth, bounds.getHeight, -bounds.getY)
    }
  var showcursor = true
  var horizontal = false
  val blink = new Timer( 500, ActionListener(_ => {showcursor = !showcursor; repaint}) )//todo: restart timer and turn on curson whenever buffer is mutated
  var curpos = Pos( 0, 0 )
  var start = Pos( 0, 0 )
  var end = Pos( 0, 0 )
  var selection = false

  buffer addView this
  blink.start

  preferredSize = ((width*cols).toInt, (height*rows).toInt)
  background = DARK_GRAY.darker
  foreground = LIGHT_GRAY
  peer.setFocusTraversalKeysEnabled( false )
  listenTo( keys )

  reactions += {
    case KeyPressed( _, _, _, _ ) =>
      blink.restart
      showcursor = true
      repaint
  }

  reactions += {
    case KeyTyped( _, '\u001B', _, _ ) =>
    case KeyTyped( _, '\u007F', _, _ ) =>
      if (selection) {
        buffer.delete( start, end )
        selection = false
        curpos = start
      } else
        buffer.delete( curpos )
    case KeyTyped( _, '\b', _, _ ) => buffer.backspace( curpos ) foreach (curpos = _)
    case KeyTyped( _, c, _, _ ) => curpos = buffer.insert( c.toString, curpos )
    case KeyPressed( _, Key.Left, m, _ ) =>
      if (m == 0)
        selection = false

      buffer.left( curpos ) foreach (curpos = _)

      if (m == Key.Modifier.Shift && !selection) {
        start = curpos
        selection = true
      }

      if (m == Key.Modifier.Shift && selection)
        end = curpos

      repaint
    case KeyPressed( _, Key.Right, m, _ ) =>
      if (m == 0)
        selection = false

      if (m == Key.Modifier.Shift && !selection) {
        start = curpos
        selection = true
      }

      if (m == Key.Modifier.Shift && selection && buffer.right( curpos ).nonEmpty)
        end = curpos

      buffer.right( curpos ) foreach (curpos = _)
      repaint
    case KeyPressed( _, Key.Up, m, _ ) =>
      buffer.up( curpos ) foreach (curpos = _)
      repaint
    case KeyPressed( _, Key.Down, m, _ ) =>
      buffer.down( curpos ) foreach (curpos = _)
      repaint
   }

  focusable = true
  requestFocus

  override def paintComponent( g: Graphics2D ): Unit = {

    super.paintComponent( g )
    g.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )

    val extract = buffer.extract( 0, buffer.rows min 25 )

    if (selection) {
      def box( row: Int, from: Int, to: Int ): Unit = {
        g fill new Rectangle2D.Double( from*width, row*height, (to - from + 1)*width, height )
      }

      val (from, to) = if (start <= end) (start, end) else (end, start)
      val c = g.getColor

      g setColor BLUE

      if (from.row == to.row)
        box( from.row, from.col, to.col )
      else {
        box( from.row, from.col, extract.rows(from.row)._1 )
        box( to.row, 0, to.col )

        for (r <- from.row + 1 until to.row)
          box( r, 0, extract.rows(r)._1)
      }

      g setColor c
    }

    var rowCount = 0

    for ((_, l) <- extract.rows) {
      var colCount = 0

      for (r <- l) {
        if (drawboxes)
          g draw new Rectangle2D.Double( width*colCount, height*rowCount, r.getNumGlyphs*width, height )

        g.drawGlyphVector( r, (width*colCount).toFloat, (height*rowCount + ascent).toFloat )
        colCount += r.getNumGlyphs
      }

      rowCount += 1
    }

    val x = curpos.col*width

    if (showcursor)
      if (horizontal) {
        val y = curpos.row*height

        g.setStroke( new BasicStroke(2.5F) )
        g.draw( new Line2D.Double(x, y, x, y + height) )
      } else {
        val y = curpos.row*height + ascent + 2

        g.setStroke( new BasicStroke(3) )
        g.draw( new Line2D.Double(x, y, x + width, y) )
      }
  }

}
//@
package xyz.hyperreal.text

import java.awt.{Color, Font}
import java.awt.font.{FontRenderContext, GlyphVector}

import collection.mutable.ArrayBuffer


class TextBuffer( val font: Font, val frc: FontRenderContext ) {

  private val views = new ArrayBuffer[TextPanel]

  def addView( v: TextPanel ) = views += v

  def removeView( v: TextPanel ) = views -= v

  private val lines = new ArrayBuffer[Line]

  lines += blankLine

  private class Line( val chars: ArrayBuffer[Char]/*, styles: ArrayBuffer[Style]*/, val runs: ArrayBuffer[GlyphVector] )

  private case class Style( length: Int, color: Color, italic: Boolean, bold: Boolean, underline: Boolean, blink: Boolean )

  def show: Unit = {

    for (l <- lines) {
      println( l.chars )
      println( l.runs )
    }

  }

  def rows = lines.length

  private def blankLine = new Line( new ArrayBuffer[Char], new ArrayBuffer[GlyphVector] )

  private def check( row: Int, col: Int ) {
    require( row >= 0, "negative row" )
    require( row < lines.length, "row is beyond last line" )
    require( col >= 0, "negative column" )
    require( col <= lines(row).chars.length, "column is beyond end of line" )
  }

  def endOfLine( row: Int, col: Int ) = col == lines(row).chars.length

  def lastLine( row: Int ) = row == lines.length - 1

  def repaint( row1: Int, row2: Int ): Unit = {
    for (v <- views)
      v.repaint()
  }

  def insertEach( s: String, row: Int, col: Int ) {
    def _insert( idx: Int, r: Int, c: Int ): Unit = {
      if (idx < s.length) {
        insertGlyphs( s(idx), r, c )

        val (r1, c1) = next( r, c ).get

        _insert( idx + 1, r1, c1 )
      }
    }

    _insert( 0, row, col )
  }

  def insert( s: String, row: Int, col: Int ): Unit = {
    val parts = new ArrayBuffer[String]

    def separate( from: Int ): Unit =
      s indexOf ('\n', from) match {
        case -1 => parts += s substring from
        case idx =>
          parts += s.substring( from, idx )
          separate( idx + 1 )
      }

    separate( 0 )

    for (i <- parts.indices) {

    }
  }

  def newline( row: Int, col: Int ): Unit = {
    check( row, col )

    val line = lines(row)

    if (endOfLine( row, col ) && lastLine( row ))
      lines += blankLine
    else if (col == 0)
      lines.insert( row, blankLine )
    else {
      find( row, col ) match {
       case (run, 0) =>
         lines.insert( row + 1, new Line(line.chars.slice(col, line.chars.length), line.runs.slice(run, line.runs.length)) )
         line.chars.remove( col, line.chars.length - col )
         line.runs.remove( run, line.runs.length - run )
//         case (run, offset) =>
      }
    }

    repaint( row, lines.length - 1 )

  }

  def find( row: Int, col: Int ): (Int, Int) = {
    var count = 0
    val runs = lines(row).runs.length

    for (i <- 0 until runs) {
      val len = lines(row).runs(i).getNumGlyphs

      if (count <= col && col < count + len)
        return (i, col - count)
      else
        count += len
    }

    (runs, 0)
  }

  def insertGlyphs( c: Char, row: Int, col: Int ) {
    check( row, col )

    val line = lines(row)

    line.chars.insert( col, c )

    find( row, col ) match {
      case (run, 0) =>
        line.runs.insert( run, font.createGlyphVector(frc, c.toString) )
       case (run, offset) =>
        line.runs.insert( run + 1, font.createGlyphVector(frc, c.toString) )
        line.runs.insert( run + 2, font.createGlyphVector(frc, line.chars.view(col, line.runs(run).getNumGlyphs - offset).toArray) )
        line.runs(run) = font.createGlyphVector( frc, line.chars.view(col - offset, col).toArray )
    }

    repaint( row, row )
//      Extract( row, col, line.runs.view(insertion, line.runs.length).toList )
  }

  def overwrite( c: Char, row: Int, col: Int ): Unit = {

  }

  def delete( row: Int, col: Int ): Unit = {

  }

  def prev( row: Int, col: Int ): Option[(Int, Int)] = {
    null
  }

  def next( row: Int, col: Int ): Option[(Int, Int)] =
    if (endOfLine( row, col ))
      if (lastLine( row ))
        None
      else
        Some( (row + 1, 0) )
    else
      Some( (row, col + 1) )

  def backspace( row: Int, col: Int ): Boolean = {
    prev( row, col ) match {
      case None => false
      case Some( (r, c) ) =>
        delete( r, c )
        true
    }
  }

  def extract( row: Int, len: Int ) = {
    check( row, 0 )
    check( row + len - 1, 0 )
    Extract( row, lines.view(row, row + len).map(_.runs.toList).toList )
  }
}

case class Extract( row: Int, rows: List[List[GlyphVector]] )
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

  def startOfLine( pos: Pos ) = pos.col == 0

  def endOfLine( pos: Pos ) = pos.col == lines(pos.row).chars.length

  def firstLine( pos: Pos ) = pos.row == 0

  def lastLine( pos: Pos ) = pos.row == lines.length - 1

  def repaint( row1: Int, row2: Int ): Unit = {
    for (v <- views)
      v.repaint
  }

  def insertEach( s: String, pos: Pos ): Pos = {
    def _insert( idx: Int, p: Pos ): Pos =
      if (idx < s.length)
        _insert( idx + 1, insertGlyphs(s(idx).toString, p) )
      else
        p

    _insert( 0, pos )
  }

  def insert( s: String, pos: Pos ): Pos = {
    val Pos(row, col) = pos

    check( row, col )

    val parts = new ArrayBuffer[String]

    def separate( from: Int ): Unit =
      s indexOf ('\n', from) match {
        case -1 => parts += s substring from
        case idx =>
          parts += s.substring( from, idx )
          separate( idx + 1 )
      }

    separate( 0 )

    var nextpos = pos

    for (i <- parts.indices) {
      if (parts(i) nonEmpty)
        nextpos = insertGlyphs( parts(i), nextpos )

      if (i < parts.length - 1)
        nextpos = newline( nextpos )
    }

    nextpos
  }

  def newline( pos: Pos ): Pos = {
    val Pos(row, col) = pos

    check( row, col )

    val line = lines(row)

    if (endOfLine( pos ) && lastLine( pos ))
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
    Pos( row + 1, 0 )
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

  def insertGlyphs( s: String, pos: Pos ): Pos = {
    val Pos(row, col) = pos

    check( row, col )

    val line = lines(row)

    line.chars.insertAll( col, s )

    find( row, col ) match {
      case (run, 0) =>
        line.runs.insert( run, font.createGlyphVector(frc, s) )
       case (run, offset) =>
        line.runs.insert( run + 1, font.createGlyphVector(frc, s) )
        line.runs.insert( run + 2, font.createGlyphVector(frc, line.chars.view(col, line.runs(run).getNumGlyphs - offset).toArray) )
        line.runs(run) = font.createGlyphVector( frc, line.chars.view(col - offset, col).toArray )
    }

    repaint( row, row )
    Pos( row, col + s.length )
  }

  def overwrite( c: Char, pos: Pos ): Unit = {

  }

  def delete( pos: Pos ): Unit = {

  }

  def left( pos: Pos ): Option[Pos] =
    if (startOfLine( pos ))
      if (firstLine( pos ))
        None
      else
        Some( Pos(pos.row - 1, lines(pos.row - 1).chars.length) )
    else
      Some( Pos( pos.row, pos.col - 1) )

  def right( pos: Pos ): Option[Pos] =
    if (endOfLine( pos ))
      if (lastLine( pos ))
        None
      else
        Some( Pos(pos.row + 1, 0) )
    else
      Some( Pos(pos.row, pos.col + 1) )

  def up( pos: Pos ): Option[Pos] =
    if (firstLine( pos ))
      None
    else
      Some( Pos(pos.row - 1, pos.col min lines(pos.row - 1).chars.length) )

  def down( pos: Pos ): Option[Pos] =
    if (lastLine( pos ))
      None
    else
      Some( Pos(pos.row + 1, pos.col min lines(pos.row + 1).chars.length) )

  def backspace( pos: Pos ): Option[Pos] =
    left( pos ) match {
      case None => None
      case res@Some( p ) =>
        delete( p )
        res
    }

  def extract( row: Int, len: Int ) = {
    check( row, 0 )
    check( row + len - 1, 0 )
    Extract( row, lines.view(row, row + len).map(_.runs.toList).toList )
  }
}

case class Pos( row: Int, col: Int )
case class Extract( row: Int, rows: List[List[GlyphVector]] )
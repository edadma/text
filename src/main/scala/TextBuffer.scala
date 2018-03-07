//@
package xyz.hyperreal.text

import java.awt.{Color, Font}
import java.awt.font.{FontRenderContext, GlyphVector}

import collection.mutable.ArrayBuffer


class TextBuffer( val font: Font, val frc: FontRenderContext ) {

  private val views = new ArrayBuffer[TextPanel]

  def addView( v: TextPanel ) = views += v

  def removeView( v: TextPanel ) = views -= v

  private var tabs = 4

  def tabStops = tabs

  def tabStops_=( t: Int ): Unit = {
    require( 0 < t && t <= 80, s"tabstop out of range: $t" )
    tabs = t
  }

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

  private def check( pos: Pos ): (Int, Int) = {
    val Pos( row, col ) = pos

    require( row >= 0, "negative row" )
    require( row < lines.length, "row is beyond last line" )
    require( col >= 0, "negative column" )
    require( col <= lines(row).chars.length, "column is beyond end of line" )
    (row, col)
  }

  private def find( pos: Pos ): (Int, Int) = {
    var count = 0
    val runs = lines(pos.row).runs.length

    for (i <- 0 until runs) {
      val len = lines(pos.row).runs(i).getNumGlyphs

      if (count <= pos.col && pos.col < count + len)
        return (i, pos.col - count)
      else
        count += len
    }

    (runs, 0)
  }

  def startOfLine( pos: Pos ) = pos.col == 0

  def endOfLine( pos: Pos ) = pos.col == lines(pos.row).chars.length

  def endOfDocument( pos: Pos ) = endOfLine( pos ) && lastLine( pos )

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
    check( pos )

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

  def tab( pos: Pos ) = insertGlyphs( " "*(tabs - pos.col%tabs), pos )

  def newline( pos: Pos ): Pos = {
    val (row, col) = check( pos )

    val line = lines(row)

    if (endOfDocument( pos ))
      lines += blankLine
    else if (col == 0)
      lines.insert( row, blankLine )
    else {
      find( pos ) match {
       case (run, 0) =>
         lines.insert( row + 1, new Line(line.chars.slice(col, line.chars.length), line.runs.slice(run, line.runs.length)) )
         line.chars.remove( col, line.chars.length - col )
         line.runs.remove( run, line.runs.length - run )
       case (run, offset) =>
          sys.error( "not yet" )
      }
    }

    repaint( row, lines.length - 1 )
    Pos( row + 1, 0 )
  }

  def insertGlyphs( s: String, pos: Pos ): Pos = {
    val (row, col) = check( pos )
    val line = lines(row)

    find( pos ) match {
      case (run, 0) =>
        line.runs.insert( run, font.createGlyphVector(frc, s) )
      case (run, offset) =>
        line.runs.insert( run + 1, font.createGlyphVector(frc, s) )
        line.runs.insert( run + 2, font.createGlyphVector(frc, line.chars.view(col, col + line.runs(run).getNumGlyphs - offset).toArray) )
        line.runs(run) = font.createGlyphVector( frc, line.chars.view(col - offset, col).toArray )
    }

    line.chars.insertAll( col, s )
    repaint( row, row )
    Pos( row, col + s.length )
  }

  def overwrite( c: Char, pos: Pos ): Unit = {

  }

  def check( from: Pos, to: Pos ): Unit =
    require( if (from.row == to.row) from.col <= to.col else from.row <= to.row, s"$from is not before $to" )

  def delete( pos: Pos ): Unit = delete( pos, pos )

  def removeLeftRun( row: Int, run: Int, offset: Int ): Unit = {

  }

  def delete( from: Pos, to: Pos ): Unit = {
    check( from, to )

    if (!endOfDocument( from )) {
      val to1 = if (endOfDocument( to )) left( to ).get else to

      if (from.row == to1.row) {
        val (fr, fo) = find( from )
        val (tr, to) = find( to1 )

        if (fr == tr) {

        } else {
          sys.error( "not yet" )
        }
      } else {
        sys.error( "not yet" )
      }
    }
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
    require( 0 <= row && row < lines.length, s"row index out of range: $row" )
    Extract( row, lines.view(row, row + len).map(_.runs.toList).toList )
  }
}

case class Pos( row: Int, col: Int )
case class Extract( row: Int, rows: List[List[GlyphVector]] )
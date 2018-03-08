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

  private case class Line( chars: ArrayBuffer[Char]/*, styles: ArrayBuffer[Style]*/, runs: ArrayBuffer[GlyphVector] )

  private case class Style( length: Int, color: Color, italic: Boolean, bold: Boolean, underline: Boolean, blink: Boolean )

  def show: Unit = {

    for (l <- lines) {
      println( l.chars )
      println( l.runs )
    }

  }

  def rows = lines.length

  private def blankLine = Line( new ArrayBuffer[Char], new ArrayBuffer[GlyphVector] )

  private def check( pos: Pos ): (Int, Int) = {
    val Pos( row, col ) = pos

    require( row >= 0, "negative row" )
    require( row < lines.length, "row is beyond last line" )
    require( col >= 0, "negative column" )
    require( col <= lines(row).chars.length, "column is beyond end of line" )
    (row, col)
  }

  private def check( from: Pos, to: Pos ): Unit = require( from <= to, s"$from is not before $to" )

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

  private def separate( s: String, c: Char ) = {
    def separate( from: Int, parts: Vector[String] ): Vector[String] =
      s.indexOf( c, from ) match {
        case -1 => parts :+ (s substring from)
        case idx => separate( idx + 1, parts :+ s.substring( from, idx ) )
      }

    separate( 0, Vector() )
  }

  private def startOfLine( pos: Pos ) = pos.col == 0

  private def endOfLine( pos: Pos ) = pos.col == lines(pos.row).chars.length

  private def endOfDocument( pos: Pos ) = endOfLine( pos ) && lastLine( pos )

  private def firstLine( pos: Pos ) = pos.row == 0

  private def lastLine( pos: Pos ) = pos.row == lines.length - 1

  def repaint( row1: Int, row2: Int ): Unit = {
    for (v <- views)
      v.repaint
  }

  def insertEach( s: String, pos: Pos ): Pos = {
    def insert( idx: Int, p: Pos ): Pos =
      if (idx < s.length)
        insert( idx + 1, glyphs(s(idx).toString, p) )
      else
        p

    val res = insert( 0, pos )
    repaint( pos.row, res.row )
    res
  }

  def insert( s: String, pos: Pos ): Pos = {
    check( pos )

    val parts = separate( s, '\n' )
    var nextpos = pos

    for (i <- parts.indices) {
      if (parts(i) nonEmpty) {
        val subparts = separate( parts(i), '\t' )

        for (j <- subparts.indices) {
          if (subparts nonEmpty)
            nextpos = glyphs( subparts(j), nextpos )

          if (j < subparts.length - 1)
            nextpos = tab( nextpos )
        }
      }

      if (i < parts.length - 1)
        nextpos = newline( nextpos )
    }

    repaint( pos.row, nextpos.row )
    nextpos
  }

  private def tab( pos: Pos ) = glyphs( " "*(tabs - pos.col%tabs), pos )

  private def newline( pos: Pos ) = {
    val (row, col) = check( pos )
    val line = lines(row)

    if (endOfDocument( pos ))
      lines += blankLine
    else if (col == 0)
      lines.insert( row, blankLine )
    else {
      find( pos ) match {
        case (run, 0) =>
          lines.insert( row + 1, Line(line.chars.slice(col, line.chars.length), line.runs.slice(run, line.runs.length)) )
          line.chars.remove( col, line.chars.length - col )
          line.runs.remove( run, line.runs.length - run )
        case (run, offset) =>
          lines.insert( row + 1,
            Line(
              line.chars.slice(col, line.chars.length),
                line.runs(run).getFont.createGlyphVector(frc, line.chars.slice(col, col + line.runs(run).getNumGlyphs - offset).toArray) +:
                  line.runs.slice(run + 1, line.runs.length)) )
          line.runs(run) = line.runs(run).getFont.createGlyphVector( frc, line.chars.slice(col - offset, col).toArray )
          line.chars.remove( col, line.chars.length - col )
          line.runs.remove( run + 1, line.runs.length - run - 1 )
      }
    }

    repaint( row, lines.length - 1 )
    Pos( row + 1, 0 )
  }

  private def glyphs( s: String, pos: Pos ): Pos = {
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
    Pos( row, col + s.length )
  }

  def overwrite( c: Char, pos: Pos ): Unit = {

  }

  def delete( pos: Pos ): Unit = delete( pos, pos )

  private def removeLeftRun( from: Pos, run: Int, offset: Int ): Unit = {
    val line = lines(from.row)

    line.runs(run) = line.runs(run).getFont.createGlyphVector( frc,
      line.chars.view(from.col + offset + 1, from.col + line.runs(run).getNumGlyphs).toArray )
    line.chars.remove( from.col, offset + 1 )
  }

  private def removeMidRun( from: Pos, run: Int, offset: Int, len: Int ): Unit = {
    val line = lines(from.row)

    line.runs(run) = line.runs(run).getFont.createGlyphVector( frc,
      (line.chars.view(from.col - offset, from.col) ++
        line.chars.view(from.col + len, from.col + line.runs(run).getNumGlyphs)).toArray )
    line.chars.remove( from.col, len )
  }

  private def removeRightRun( from: Pos, run: Int, offset: Int ): Unit = {
    val line = lines(from.row)

    line.chars.remove( from.col, line.runs(run).getNumGlyphs - offset )
    line.runs(run) = line.runs(run).getFont.createGlyphVector( frc,
      line.chars.view(from.col - offset, from.col).toArray )
  }

  private def removeEntireRun( from: Pos, run: Int ): Unit = {
    val line = lines(from.row)

    line.chars.remove( from.col, line.runs(run).getNumGlyphs )
    line.runs.remove( run )
  }

  private def removeRun( from: Pos, run: Int, foff: Int, toff: Int ): Unit = {
    val line = lines(from.row)

    if (run == line.runs.length) {
      line.chars ++= lines(from.row + 1).chars
      line.runs ++= lines(from.row + 1).runs
      lines.remove( from.row + 1 )
    } else if (foff == 0 && toff == line.runs(run).getNumGlyphs - 1)
      removeEntireRun( from, run )
    else if (foff == 0)
      removeLeftRun( from, run, toff )
    else if (toff == line.runs(run).getNumGlyphs - 1)
      removeRightRun( from, run, foff )
    else
      removeMidRun( from, run, foff, toff - foff + 1 )
  }

  private def removeRuns( from: Pos, frun: Int, foff: Int, to: Pos, trun: Int, toff: Int ): Unit = {
    val line = lines(from.row)

    if (frun == trun)
      removeRun( from, frun, foff, toff )
    else {
      removeRuns( from + line.runs(frun).getNumGlyphs - foff, frun + 1, 0, to, trun, toff )
      removeRun( from, frun, foff, line.runs(frun).getNumGlyphs - 1 )
    }
  }

  def delete( from: Pos, to: Pos ): Unit = {
    check( from, to )

    if (!endOfDocument( from )) {
      val to1 = if (endOfDocument( to )) left( to ).get else to
      val (frun, foff) = find( from )
      val (trun, toff) = find( to1 )

      if (from.row == to1.row) {
        if (frun == trun)
          removeRun( from, frun, foff, toff )
        else
          removeRuns( from, frun, foff, to1, trun, toff )
      } else {
        val line = lines(from.row)
        val lrun = line.runs.length - 1

        removeRuns( from, frun, foff, Pos(from.row, lines(from.row).chars.length - 1), lrun, line.runs(lrun).getNumGlyphs - 1 )
        removeRuns( Pos(to1.row, 0), 0, 0, to1, trun, toff )
        lines.remove( from.row + 1, to1.row - from.row - 1 )
        delete( from, from )
      }
    }

    repaint( from.row, to.row )
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
    Extract( row, lines.view(row, row + len).map(l => (l.runs.aggregate(0)((x, v) => x + v.getNumGlyphs, _ + _), l.runs.toList)).toVector )
  }
}

case class Pos( row: Int, col: Int ) {
  def <=( pos: Pos ) = row < pos.row  || row == pos.row && col <= pos.col
  def +( a: Int ) = Pos( row, col + a )
  def -( a: Int ) = Pos( row, col - a )
}

case class Extract( row: Int, rows: Vector[(Int, List[GlyphVector])] )
//todo: 4th eve part-time 5144832121 2237
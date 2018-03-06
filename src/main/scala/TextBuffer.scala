//@
package xyz.hyperreal.text

import java.awt.{Color, Font}
import java.awt.font.{FontRenderContext, GlyphVector}

import collection.mutable.ArrayBuffer


class TextBuffer( val font: Font, val frc: FontRenderContext ) {

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

  }

  def insert( c: Char, row: Int, col: Int ) {
    check( row, col )

    val line = lines(row)

    line.chars.insert( col, c )

    def find: (Int, Int) = {
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

    if (c == '\n') {
      if (endOfLine( row, col ) && lastLine( row ))
        lines += blankLine
      else if (col == 0)
        lines.insert( row, blankLine )
      else {
        find match {
         case (run, 0) =>
           lines.insert( row + 1, new Line(line.chars.slice(col, line.chars.length), line.runs.slice(run, line.runs.length)) )
           line.chars.remove( col, line.chars.length - col )
           line.runs.remove( run, line.runs.length - run )
//         case (run, offset) =>
        }
      }

      repaint( row, lines.length - 1 )
    } else {
      find match {
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
  }

  def overwrite( c: Char, row: Int, col: Int ): Unit = {

  }

  def delete( row: Int, col: Int ): Unit = {

  }

  def prev( row: Int, col: Int ): Option[(Int, Int)] = {
    null
  }

  def next( row: Int, col: Int ): Option[(Int, Int)] = {
    null
  }

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
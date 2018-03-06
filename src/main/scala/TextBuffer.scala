//@
package xyz.hyperreal.text

import java.awt.{Color, Font}
import java.awt.font.{FontRenderContext, GlyphVector}

import collection.mutable.ArrayBuffer


class TextBuffer( font: Font, frc: FontRenderContext ) {

  private val lines = new ArrayBuffer[Line]

  lines += blankLine

  private case class Line( chars: ArrayBuffer[Char]/*, styles: ArrayBuffer[Style]*/, runs: ArrayBuffer[GlyphVector] )

  private case class Style( length: Int, color: Color, italic: Boolean, bold: Boolean )

  def show: Unit = {

    for (l <- lines) {
      println( l.chars )
      println( l.runs )
    }

  }

  private def blankLine = Line( new ArrayBuffer[Char], new ArrayBuffer[GlyphVector] )

  private def check( row: Int, col: Int ) {
    require( row >= 0, "negative row" )
    require( row < lines.length, "row is beyond last line" )
    require( col >= 0, "negative column" )
    require( col <= lines(row).chars.length, "column is beyond end of line" )
  }

  def eol( row: Int, col: Int ): Boolean = col == lines(row).chars.length

  def insert( c: Char, row: Int, col: Int ): Extract = {
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

    val insertion =
      find match {
        case (run, 0) =>
          line.runs.insert( run, font.createGlyphVector(frc, c.toString) )
          run
        case (run, offset) =>
          line.runs.insert( run + 1, font.createGlyphVector(frc, c.toString) )
          line.runs.insert( run + 2, font.createGlyphVector(frc, line.chars.view(col, line.runs(run).getNumGlyphs - offset).toArray) )
          line.runs(run) = font.createGlyphVector(frc, line.chars.view(col - offset, col).toArray )
          run + 1
      }

    Extract( row, col, line.runs.view(insertion, line.runs.length).toList )
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

  def extract( row: Int, col: Int, len: Int ): Extract = {
    null
  }
}

case class Extract( row: Int, col: Int, runs: List[GlyphVector] )
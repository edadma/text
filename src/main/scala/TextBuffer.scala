package xyz.hyperreal.text

import java.awt.Color
import java.awt.font.GlyphVector

import collection.mutable.ArrayBuffer


class TextBuffer {

  private val lines = new ArrayBuffer[Line]

  private class Line {
    val chars = new ArrayBuffer[Char]

    val styles = new ArrayBuffer[Style]

    val runs = new ArrayBuffer[Run]
  }

  private case class Style( length: Int, color: Color, italic: Boolean, bold: Boolean )

  private case class Run( length: Int, glyphs: GlyphVector, color: Color, italic: Boolean, bold: Boolean )

  def insert( c: Char, row: Int, col: Int ): Unit = {

  }

  def overwrite( c: Char, row: Int, col: Int ): Unit = {

  }

  def delete( row: Int, col: Int ): Unit = {

  }

  def prev( row: Int, col: Int ) = {

  }

  def next( row: Int, col: Int ) = {

  }

  def backspace( row: Int, col: Int ): Boolean = {

  }
}
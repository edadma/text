//@
package xyz.hyperreal.text


abstract class Signaling {
  protected val sequences: String
  protected val actions: Vector[Map[String, Int] => Unit]
}

object ANSI extends Signaling {
  val sequences =
    """
      |CSI n=1 A           CUU – Cursor Up 	Moves the cursor n (default 1) cells in the given direction. If the cursor is already at the edge of the screen, this has no effect.
      |CSI n=1 B           CUD – Cursor Down
      |CSI n=1 C 	         CUF – Cursor Forward
      |CSI n=1 D 	         CUB – Cursor Back
      |CSI n=1 E 	         CNL – Cursor Next Line 	Moves cursor to beginning of the line n (default 1) lines down. (not ANSI.SYS)
      |CSI n=1 F 	         CPL – Cursor Previous Line 	Moves cursor to beginning of the line n (default 1) lines up. (not ANSI.SYS)
      |CSI n=1 G 	         CHA – Cursor Horizontal Absolute 	Moves the cursor to column n (default 1). (not ANSI.SYS)
      |CSI n=1 ; m=1 H 	   CUP – Cursor Position 	Moves the cursor to row n, column m. The values are 1-based, and default to 1 (top left corner) if omitted. A sequence such as CSI ;5H is a synonym for CSI 1;5H as well as CSI 17;H is the same as CSI 17H and CSI 17;1H
      |CSI n=0 J           ED – Erase in Display 	Clears part of the screen. If n is 0 (or missing), clear from cursor to end of screen. If n is 1, clear from cursor to beginning of the screen. If n is 2, clear entire screen (and moves cursor to upper left on DOS ANSI.SYS). If n is 3, clear entire screen and delete all lines saved in the scrollback buffer (this feature was added for xterm and is supported by other terminal applications).
      |CSI n=0 K 	         EL – Erase in Line 	Erases part of the line. If n is zero (or missing), clear from cursor to the end of the line. If n is one, clear from cursor to beginning of the line. If n is two, clear entire line. Cursor position does not change.
      |CSI n=1 S    	     SU – Scroll Up 	Scroll whole page up by n (default 1) lines. New lines are added at the bottom. (not ANSI.SYS)
      |CSI n=1 T 	         SD – Scroll Down 	Scroll whole page down by n (default 1) lines. New lines are added at the top. (not ANSI.SYS)
      |CSI n=1 ; m=1 f 	   HVP – Horizontal Vertical Position 	Same as CUP
      |CSI n=0 {; ni=0} m  SGR – Select Graphic Rendition 	Sets SGR parameters, including text color. After CSI can be zero or more parameters separated with semicolon. If none, CSI m is treated as CSI 0 m (reset / normal).
      |CSI 5 i             AUX Port On 	Enable aux serial port usually for local serial printer
      |CSI 4 i             AUX Port Off 	Disable aux serial port usually for local serial printer
      |CSI 6 n             DSR – Device Status Report 	Reports the cursor position (CPR) to the application as (as though typed at the keyboard) ESC[n;mR, where n is the row and m is the column.)
      |CSI s               SCP – Save Cursor Position 	Saves the cursor position/state.
      |CSI u 	             RCP – Restore Cursor Position 	Restores the cursor position/state.
    """.stripMargin.trim
  var cursor = Pos( 0, 0 )
  val actions =
    Vector(

    )
}
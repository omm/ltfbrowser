/* Copyright 2017 Rafal Jopek ( rafaljopek at hotmail com ) */

/* Large Text File Browser, in short LTFBrowser. */

#include "fileio.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"

PROCEDURE Main( cFileName )

   LOCAL aBrowser, aPointer
   LOCAL nHandle
   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL tmp
   LOCAL nRow := 1, nCol := 1
   LOCAL nKey
   LOCAL cDelim
   LOCAL nBOF, nEOF
   LOCAL nRowCount := 1, nColCount := 1, cline, nPoin

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

   IF cFileName == NIL
      ?
      ? " Usage: xedit <cFileName>"
      ?
      ? "        where <cFileName> is the name of the file you want to browse"
      ?
      Inkey( 0 )
      RETURN
   ELSE

      IF ( nHandle := FOpen( cFileName ) ) >= 0

         IF ( cDelim := CheckEOL( nHandle ) ) == NIL
            hb_Alert( "End of line unrecognized.", , 0xf0 )
            RETURN
         ENDIF

         nEOF := FileSize( nHandle )
         nBOF := FSeek( nHandle, 0, FS_SET )
         aPointer  := { 0 }

         DO WHILE lContinue

            IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()

               Scroll()

               aBrowser := {}
               IF nBOF != 0
                  aPointer := { 0 }
                  nBOF := 1
               ELSE
                  aPointer := { FSeek( nHandle, aPointer[ 1 ], FS_RELATIVE ) }
               ENDIF

               tmp := 0
               DO WHILE tmp < MaxRow()
                  IF ATail( aPointer ) < nEOF
                     AAdd( aBrowser, FReadLineBottom( nHandle, cDelim ) )
                     AAdd( aPointer, FSeek( nHandle, 0, FS_RELATIVE ) )
                     tmp++
                  ELSE
                     EXIT
                  ENDIF
               ENDDO

               xEdit_Display( aBrowser, nRow, nCol )

               nMaxRow := MaxRow()
               nMaxCol := MaxCol()
            ENDIF

            DispBegin()
            FOR EACH cline IN aPointer
               IF cLine:__enumIndex() == nRow
                  nPoin := cLine:__enumValue()
                  EXIT
               ENDIF
            NEXT
            hb_DispOutAt( MaxRow(), 0, ;
               PadR( " Row(" + hb_ntos( nRowCount ) + ") Col(" + hb_ntos( nColCount ) + ")" + ;
               " EOL(" + hb_ValToExp( cDelim )    + ")" + ;
               " FilePointer(" + hb_ntos( nPoin ) + ")" + ;
               " FileSize(" + hb_ntos( nEOF )     + ")", MaxCol() + 1 ), 0xe0 )
            DispEnd()

            nKey := Inkey( 0, INKEY_ALL )

            SWITCH nKey

            CASE K_ESC
               lContinue := .F.
               EXIT

            CASE K_UP                                  /*   Up arrow, Ctrl-E */

               IF nRow > 1
                  nRow--
                  nRowCount--
               ELSE
                  IF aPointer[ 1 ] > 0
                     nRowCount--
                     FSeek( nHandle, aPointer[ 1 ], FS_SET )
                     hb_AIns( aBrowser, 1, FReadLineTop( nHandle, cDelim ), .F. )
                     hb_AIns( aPointer, 1, FSeek( nHandle, 0, FS_RELATIVE ), .F. )
                  ENDIF
               ENDIF
               xEdit_Display( aBrowser, nRow, nCol )
               EXIT

            CASE K_DOWN                                /*   Down arrow, Ctrl-X */

               IF nRow < Len( aBrowser )
                  nRow++
                  nRowCount++
               ELSE
                  IF ! FileEOF( nHandle )
                     nRowCount++
                     FSeek( nHandle, ATail( aPointer ), FS_SET )
                     ADel( aBrowser, 1 )
                     aBrowser[ MaxRow() ] := FReadLineBottom( nHandle, cDelim )
                     ADel( aPointer, 1 )
                     aPointer[ MaxRow() + 1 ] := FSeek( nHandle, 0, FS_RELATIVE )
                  ENDIF
               ENDIF
               xEdit_Display( aBrowser, nRow, nCol )
               EXIT

            ENDSWITCH

         ENDDO

      ELSE

         IF FError() != 0
            hb_Alert( "Cannot open file, error: ; ;" + hb_ntos( FError() ), , 0xf0 )
            RETURN
         ENDIF

      ENDIF

   ENDIF

   RETURN

STATIC PROCEDURE xEdit_Display( aBrowser, nRow, nCol )

   LOCAL cLine

   DispBegin()
   Scroll()
   FOR EACH cLine IN aBrowser
      hb_DispOutAt( cLine:__enumIndex() - 1, 0, ;
         Pad( cLine, MaxCol() + 1 ), ;
         iif( cLine:__enumIndex() == nRow, hb_NToColor( 0xf0 ), NIL ) )
   NEXT
   SetPos( nRow - 1, nCol - 1 )
   DispEnd()

   RETURN

STATIC FUNCTION FReadLineTop( nHandle, cDelim )

   LOCAL cBufferVar
   LOCAL nChrsRead
   LOCAL cLine
   LOCAL nEOLPos

   FSeek( nHandle, iif( cDelim == e"\r\n", - 2, - 1 ), FS_RELATIVE )

   nChrsRead := Min( 4096, FSeek( nHandle, 0, FS_RELATIVE ) )

   cBufferVar := Space( nChrsRead )
   FSeek( nHandle, 0 )
   FRead( nHandle, @cBufferVar, nChrsRead )
   FSeek( nHandle, 0 )
   cLine := cBufferVar

   nEOLPos := RAt( cDelim, cLine )

   cLine := SubStr( cLine, nEOLPos + 1 + iif( cDelim == e"\r\n", 1, 0 ) )
   FSeek( nHandle, nEOLPos + iif( cDelim == e"\r\n", 1, 0 ), FS_RELATIVE )

   RETURN cLine

STATIC FUNCTION FReadLineBottom( nHandle, cDelim )

   LOCAL cBufferVar
   LOCAL nChrsRead
   LOCAL cLine
   LOCAL nEOLPos

   cBufferVar := Space( 4096 )
   nChrsRead := FRead( nHandle, @cBufferVar, 4096 )
   cLine := SubStr( cBufferVar, 1, nChrsRead )

   nEOLPos := At( cDelim, cLine )

   IF nEOLPos == 0
      nEOLPos := Len( cLine ) + 1
   ENDIF

   cLine := SubStr( cLine, 1, nEOLPos - 1 )
   FSeek( nHandle, nEOLPos - nChrsRead + iif( cDelim == e"\r\n", 1, 0 ), FS_RELATIVE )

   RETURN cLine

STATIC FUNCTION CheckEOL( nHandle )

   LOCAL cBufferVar

   cBufferVar := Space( 4096 )
   FRead( nHandle, @cBufferVar, 4096 )
   FSeek( nHandle, 0, FS_SET )

   RETURN hb_StrEOL( cBufferVar )

STATIC FUNCTION FileEOF( nHandle )

   LOCAL nPos
   LOCAL nEOF

   nPos := FSeek( nHandle, 0, FS_RELATIVE )
   nEOF := FSeek( nHandle, 0, FS_END )
   FSeek( nHandle, nPos, FS_SET )

   RETURN nEOF <= nPos

STATIC FUNCTION FileSize( nHandle )

   LOCAL nPos
   LOCAL nBytes

   nPos := FSeek( nHandle, 0, FS_RELATIVE )
   nBytes := FSeek( nHandle, 0, FS_END )
   FSeek( nHandle, nPos, FS_SET )

   RETURN nBytes

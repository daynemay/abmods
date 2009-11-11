/* Test this on cn-entry.w */

DEFINE INPUT  PARAMETER hEditor AS HANDLE       NO-UNDO.
DEFINE INPUT  PARAMETER iLineNo AS INTEGER      NO-UNDO.
DEFINE OUTPUT PARAMETER cLine   AS CHARACTER    NO-UNDO.




DEFINE VARIABLE cEditorText AS LONGCHAR NO-UNDO.

/* DEFINE VARIABLE c_tempdir   AS CHARACTER NO-UNDO. */
/* DEFINE VARIABLE c_tempfile  AS CHARACTER NO-UNDO. */
/* DEFINE VARIABLE l_modified  AS LOGICAL   NO-UNDO. */
/*                                                   */
DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcNewLine AS CHARACTER NO-UNDO.

/* cn-entry.w */

lcNewLine = CHR ( IF ( OPSYS = "WIN32" )
                  THEN 10
                  ELSE 13 ).

IF hEditor:TYPE NE "EDITOR" THEN 
    RETURN.

IF hEditor:LENGTH = 0 THEN 
    RETURN.

IF iLineNo <= 0 THEN 
    iLineNo = 1.

IF iLineNo > hEditor:NUM-LINES THEN
    iLineNo = hEditor:NUM-LINES.

/* cn-entry.w */


/* We can only use SCREEN-VALUE if the editor is under 32K. Annoying. */
IF hEditor:LENGTH < 32000 THEN
DO:

    cLine = ENTRY ( iLineNo, hEditor:SCREEN-VALUE, lcNewLine ).

    RETURN.

END.


/* For large files, can't use screen value, so mess with selection instead. */
/* I want a better way to do this, since its side-effects are annoying.     */
DEFINE VARIABLE liOrigLine AS INTEGER     NO-UNDO.
DEFINE VARIABLE liOrigChar AS INTEGER     NO-UNDO.

DEFINE VARIABLE lcOrigSelectedText AS CHARACTER   NO-UNDO.

ASSIGN
    /* Save original cursor position */
    liOrigLine = hEditor:CURSOR-LINE
    liOrigChar = hEditor:CURSOR-CHAR
    
    lcOrigSelectedText = hEditor:SELECTION-TEXT

    /* Get the current line */    
    hEditor:CURSOR-LINE = iLineNo
    hEditor:CURSOR-CHAR = 1.



hEditor:SOURCE-COMMAND("select_line","").
cLine = hEditor:SELECTION-TEXT.
hEditor:SOURCE-COMMAND("deselect","").

ASSIGN    
    /* Restore original cursor position */
    hEditor:CURSOR-LINE = liOrigLine.
    hEditor:CURSOR-CHAR = liOrigChar.

IF lcOrigSelectedText <> "" THEN
DO:
    /* Mess around to restore the selection-text as well  */
    hEditor:CURSOR-CHAR = 1.
    hEditor:SEARCH ( lcOrigSelectedText, FIND-NEXT-OCCURRENCE + FIND-SELECT  ).
    hEditor:CURSOR-CHAR = liOrigChar.
    
END.
    

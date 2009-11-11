DEFINE INPUT  PARAMETER phWindow        AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER plTrimDots      AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER pcSelectedText  AS CHARACTER NO-UNDO.
{linxade/winprocs.i}
{linxade/nonzeromin.i}

DEFINE VARIABLE c_line       AS CHARACTER  NO-UNDO. /* Current line in editor */
DEFINE VARIABLE c_word       AS CHARACTER  NO-UNDO. /* Current word in editor */

DEFINE VARIABLE c_tempdir    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c_tempfile   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE l_modified   AS LOGICAL    NO-UNDO. /* Changed editor contents? */

DEFINE VARIABLE i            AS INTEGER    NO-UNDO.

DEFINE VARIABLE i_WordStart  AS INTEGER    NO-UNDO. /* c_word start  */
DEFINE VARIABLE i_WordLength AS INTEGER    NO-UNDO. /* c_word length */
DEFINE VARIABLE i_WordEnd    AS INTEGER    NO-UNDO. /* c_word end    */

DEFINE VARIABLE lhEditor AS HANDLE      NO-UNDO.
DEFINE VARIABLE llFound  AS LOGICAL     NO-UNDO.

RUN findWidgetOfType
    ( INPUT phWindow,
      INPUT "EDITOR",
      OUTPUT lhEditor,
      OUTPUT llFound ).

IF NOT llFound THEN
    RETURN.

IF lhEditor:TYPE NE "EDITOR" THEN
    RETURN.

/* Determine where we we should drop our temporary file.  Look for */
/* this program, and make a temporary file in the same directory.  */
c_tempdir = RIGHT-TRIM( SEARCH( PROGRAM-NAME (1) ), PROGRAM-NAME (1) ).
c_tempdir = REPLACE ( c_tempdir, "~\", "/").
c_tempdir = RIGHT-TRIM ( c_tempdir, "/" ).

c_tempfile = c_tempdir + "/tempfile.p".

/* Write the contents of the procedure window out to a temporary file.  */
/* We then read the file line by line until we get to where the cursor  */
/* is positioned.  It has to be done this way, since we can't deal with */
/* the EDITOR:SCREEN-VALUE if there is over 32K in the text area.       */
l_modified = lhEditor:MODIFIED.
IF NOT lhEditor:SAVE-FILE ( c_tempfile ) THEN RETURN.
lhEditor:MODIFIED = l_modified. /* Because SAVE-FILE() sets MODIFIED to FALSE */

INPUT FROM VALUE ( c_tempfile ).
DO i = 1 TO lhEditor:CURSOR-LINE ON END-KEY UNDO, LEAVE:

  IMPORT UNFORMATTED c_line NO-ERROR.

END.
INPUT CLOSE.



IF OPSYS = "WIN32" THEN
  c_tempfile = REPLACE ( c_tempfile, "/", "~\" ).

/* Delete the temporary file */
OS-DELETE VALUE ( c_tempfile ).

/* Trouble deleting the temporary file? */
IF OS-ERROR NE 0 THEN
    MESSAGE
        "Error #"
        STRING(OS-ERROR,"99")
        " occurred while deleting temporary file." SKIP
        " You will have to manually delete " +
        c_tempfile + "."
        VIEW-AS ALERT-BOX.

/* Look for the next space or punctuation char to the right of the cursor */
i_WordEnd = NON-ZERO-MIN (
                 STRING ( INDEX ( c_line, " ",  lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "~}", lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "~{", lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "'",  lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, '"',  lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "(",  lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, ")",  lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, ",",  lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, ":",  lhEditor:CURSOR-CHAR ) ) + "," +
                 STRING ( INDEX ( c_line, "~~", lhEditor:CURSOR-CHAR ) )
                ).

IF i_WordEnd = 0 THEN i_WordEnd = LENGTH ( c_line ) + 1.

/* Trim off everything past the end of the current word  */
c_line       = TRIM(SUBSTRING(c_line, 1, i_WordEnd)).

/* TODO: Delete */ MESSAGE "c_line" SKIP c_line SKIP
    SELF:CURSOR-CHAR
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* The space or punctuation character before the current word */
i_WordStart  = 1 + MAX (
                        R-INDEX ( c_line, " " , lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, "~}", lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, "~{", lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, "'" , lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, '"' , lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, "(" , lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, ")" , lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, "," , lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, "~~", lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, ":",  lhEditor:CURSOR-CHAR ),
                        R-INDEX ( c_line, "=" , lhEditor:CURSOR-CHAR )
                       ).

i_WordLength = i_WordEnd - i_WordStart.

/* TODO: Delete */ 
    MESSAGE 
        "Start: " i_WordStart SKIP 
        "End: " i_WordEnd SKIP 
        "Length: " i_WordLength
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

c_word = SUBSTRING(c_Line, i_WordStart, i_WordLength). /* Get the word */

/* TODO: Delete */ MESSAGE "here : :: " c_word
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Trim punctuation */

c_word = LEFT-TRIM  ( c_word, "~{" ).       /* Allow includes */
c_word = RIGHT-TRIM ( c_word, "~}" ).       /* Allow includes */
c_word = TRIM       ( c_word, '"'  ).
c_word = TRIM       ( c_word, "'"  ).
c_word = REPLACE ( c_word, "/" + "*", "").  /* Open comments */
c_word = REPLACE ( c_word, "*" + "/", "").  /* Close comments */
c_word = TRIM       ( c_word, '('  ).
c_word = TRIM       ( c_word, ')'  ).

IF plTrimDots THEN
DO:
    ASSIGN
        c_word = RIGHT-TRIM ( c_word, "." )
        c_word = RIGHT-TRIM ( c_word, "," ).
END.

pcSelectedText = c_word.
